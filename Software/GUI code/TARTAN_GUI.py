import ctypes
ctypes.windll.shcore.SetProcessDpiAwareness(1)

import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import threading
import queue
import time
import csv
import os
from pathlib import Path
import serial
from collections import deque

from PIL import Image, ImageTk

# Load image assets relative to this script, not the launch directory.
ASSET_DIR = Path(__file__).resolve().parent

# ==========================
# USER CONFIG
# ==========================
DEVICE_NAME     = "TARTAN"
DEVICE_SUBTITLE = "Transfer Apparatus for Repetitive Tactile Application and Normalisation"

DEFAULT_PORT_UNO  = "COM17"   # Uno  (motors)   — update to match Device Manager
DEFAULT_PORT_NANO = "COM16"   # Nano (load cell) — update to match Device Manager
DEFAULT_BAUD      = 115200

STEPS_PER_MM       = 400.0
MOVE_START_DELAY_S = 1.0

LRCFS_LOGO_CANDIDATES      = ["lrcfs.png", "lcrfs.png", "LRCFS.png", "LCRFS.png"]
LEVERHULME_LOGO_CANDIDATES = ["leverhulme.png", "Leverhulme.png"]

RAW_N_DEFAULT = 20
HILITE_BG     = "#cfe8ff"
HILITE_FG     = "#0b3d91"

ALLOWED_SPEEDS = [0.5, 1.0, 1.5, 2.0, 2.5]
SPEED_TOL      = 1e-6


def validate_speed(speed: float) -> float:
    speed = round(float(speed), 1)
    if not any(abs(speed - s) <= SPEED_TOL for s in ALLOWED_SPEEDS):
        raise ValueError(
            f"Speed {speed:.1f} cm/s not allowed. "
            f"Allowed: {', '.join(f'{s:.1f}' for s in ALLOWED_SPEEDS)}"
        )
    return speed


# ==========================
# Helpers
# ==========================
def cm_to_mm(dist_cm: float) -> float:
    return dist_cm * 10.0

def today_yyyymmdd() -> str:
    return time.strftime("%Y%m%d")

def mean(values):
    return sum(values) / len(values)

def sample_sd(values):
    if len(values) < 2:
        return 0.0
    m   = mean(values)
    var = sum((x - m) ** 2 for x in values) / (len(values) - 1)
    return var ** 0.5


# ==========================
# Scrollable helper
# ==========================
def make_scrollable_root(parent: tk.Tk):
    container = ttk.Frame(parent)
    container.pack(side="top", fill="both", expand=True)

    canvas  = tk.Canvas(container, highlightthickness=0, bd=0, bg="#ffffff")
    vscroll = ttk.Scrollbar(container, orient="vertical", command=canvas.yview,
                            style="Thin.Vertical.TScrollbar")
    canvas.configure(yscrollcommand=vscroll.set)
    canvas.pack(side="left", fill="both", expand=True)

    inner     = ttk.Frame(canvas)
    window_id = canvas.create_window((0, 0), window=inner, anchor="nw")

    def _update_scrollbar_visibility():
        canvas.update_idletasks()
        bbox = canvas.bbox("all")
        if not bbox:
            if vscroll.winfo_ismapped(): vscroll.pack_forget()
            return
        if bbox[3] - bbox[1] > canvas.winfo_height() + 2:
            if not vscroll.winfo_ismapped(): vscroll.pack(side="right", fill="y")
        else:
            if vscroll.winfo_ismapped():
                vscroll.pack_forget()
                canvas.yview_moveto(0)

    inner.bind("<Configure>",  lambda e: (canvas.configure(scrollregion=canvas.bbox("all")),
                                          _update_scrollbar_visibility()))
    canvas.bind("<Configure>", lambda e: (canvas.itemconfigure(window_id, width=canvas.winfo_width()),
                                          _update_scrollbar_visibility()))
    parent.after(200, _update_scrollbar_visibility)
    canvas.bind_all("<MouseWheel>", lambda e: canvas.yview_scroll(int(-1*(e.delta/120)), "units"))

    return container, inner


# ==========================
# Serial worker
# ==========================
class SerialWorker(threading.Thread):
    def __init__(self, port, baud, rx_queue):
        super().__init__(daemon=True)
        self.port       = port
        self.baud       = baud
        self.q          = rx_queue
        self.stop_event = threading.Event()
        self.ser        = None
        self.lock       = threading.Lock()

    def run(self):
        try:
            self.ser = serial.Serial(self.port, self.baud, timeout=1)
            time.sleep(1.0)
            self.q.put(("status", "connected"))
        except Exception as e:
            self.q.put(("status_msg", f"ERROR opening {self.port}: {e}"))
            self.q.put(("status", "disconnected"))
            return

        while not self.stop_event.is_set():
            try:
                line = self.ser.readline()
                if not line: continue
                s = line.decode("utf-8", errors="ignore").strip()
                if s: self.q.put(("line", s))
            except Exception as e:
                self.q.put(("status_msg", f"Serial error: {e}"))
                self.q.put(("status", "disconnected"))
                break

        try:
            if self.ser and self.ser.is_open: self.ser.close()
        except: pass
        self.q.put(("status", "disconnected"))

    def stop(self):
        self.stop_event.set()

    def send(self, cmd: str):
        if not self.ser or not self.ser.is_open: return
        with self.lock:
            print("TX >", cmd.strip())
            self.ser.write((cmd.strip() + "\n").encode("utf-8", errors="ignore"))


# ==========================
# GUI
# ==========================
class App(tk.Tk):
    def __init__(self):
        super().__init__()
        style = ttk.Style(self)
        try: style.theme_use("clam")
        except: pass

        WHITE = "#ffffff"
        self.tk.call("tk", "scaling", 2)
        for s in (".", "TFrame", "TLabelframe", "TLabelframe.Label", "TLabel", "TButton"):
            style.configure(s, background=WHITE)
        style.configure("Thin.Vertical.TScrollbar",
                        gripcount=0, arrowsize=12, width=10, borderwidth=0,
                        relief="flat", background="#d6d6d6", troughcolor="#f3f3f3")
        style.configure("Info.TButton", relief="flat", borderwidth=0, padding=0)
        style.map("Info.TButton", relief=[("pressed","flat"),("active","flat")])
        self.configure(bg=WHITE)
        self.title(DEVICE_NAME)
        self.state("zoomed")

        # Serial workers and queues
        self.uno_q    = queue.Queue()
        self.uno_worker  = None
        self.nano_q   = queue.Queue()
        self.nano_worker = None

        # Nano force buffer: deque of (wall_time_s, force_g)
        self.nano_buffer = deque(maxlen=50)

        self.info_icon = self._load_small_icon("info.jpg", size=25)

        # Live state
        self.live = {
            "force_g": None,
            "mode":    "IDLE",
        }

        # Calibration (scale) responses come from Nano
        self.latest_raw   = None
        self.latest_scale = None

        # Logging
        self.logging             = False
        self.log_fp              = None
        self.log_writer          = None
        self.move_after_id       = None
        self.run_active          = False
        self.run_started_move    = False
        self.run_moving          = False
        self.run_total_steps     = 0      # total steps for current move
        self.run_move_start_time = None   # time.time() when MOVE_START received
        self.run_steps_per_sec   = 0.0    # steps/s for current move
        self.uno_issued_steps = 0  # Start each CSV run at zero pulses, not the previous move
        self.uno_move_started = False  # Ignore stale STEP_COUNT before this MOVE_START
        self.run_post_completion_samples = 0
        self.run_completion_received = False
        self._auto_finish_id     = None   # fallback timer if MOVE_DONE never arrives
        self.run_out_dir         = None
        self.run_base_name       = tk.StringVar(value="")

        self.cal_status  = tk.StringVar(value="Click 'Start calibration check' to begin.")
        self.run_status  = tk.StringVar(value="Idle.")
        self.tare_status = tk.StringVar(value="")

        # Platform position calibration workflow
        self.hh_cal_active     = False
        self.hh_cal_stage      = None
        self.hh_cal_seen_motion = False

        # Tare workflow
        self.raw0 = None
        self.raw1 = None

        # Multi-mass calibration check
        self.cal_weights     = []
        self.cal_index       = 0
        self.cal_running     = False
        self.cal_results     = []
        self.cal_raw_results = []

        self._outer_container, self.page = make_scrollable_root(self)
        self._build_ui(self.page)

        self.live_section = ttk.LabelFrame(self, text="")
        self.live_section.pack(side="bottom", fill="x", padx=12, pady=(0, 12))
        self._add_highlight(self.live_section, "Live reading")
        self._build_live_strip(self.live_section)

        self.after(50, self._poll_serial)
        self.protocol("WM_DELETE_WINDOW", self.on_close)

    # ==========================
    # Responsive layout helpers
    # ==========================
    def _layout_tare_responsive(self, event=None):
        if not hasattr(self, "tare_row"): return
        needed = ("btn_raw0","btn_raw1","btn_set_save_scale","btn_load_scale_file",
                  "btn_info_raw0","btn_info_raw1","btn_info_scale")
        if not all(hasattr(self, n) for n in needed): return
        w      = self.tare_row.winfo_width()
        WRAP_AT = 950
        if w < WRAP_AT:
            self.btn_raw0.grid_configure(row=1, column=0, padx=(0,6), pady=(0,6), sticky="w")
            self.btn_info_raw0.grid_configure(row=1, column=1, padx=(0,10), pady=(0,6), sticky="nw")
            self.btn_raw1.grid_configure(row=1, column=2, padx=(0,6), pady=(0,6), sticky="w")
            self.btn_info_raw1.grid_configure(row=1, column=3, padx=(0,10), pady=(0,6), sticky="nw")
            self.btn_set_save_scale.grid_configure(row=1, column=4, padx=(0,6), pady=(0,6), sticky="w")
            self.btn_info_scale.grid_configure(row=1, column=5, padx=(0,10), pady=(0,6), sticky="nw")
            self.btn_load_scale_file.grid_configure(row=1, column=6, padx=(0,6), pady=(0,6), sticky="w")
        else:
            self.btn_raw0.grid_configure(row=0, column=6, padx=(0,6), pady=6, sticky="w")
            self.btn_info_raw0.grid_configure(row=0, column=7, padx=(0,10), pady=(0,6), sticky="nw")
            self.btn_raw1.grid_configure(row=0, column=8, padx=(0,6), pady=6, sticky="w")
            self.btn_info_raw1.grid_configure(row=0, column=9, padx=(0,10), pady=(0,6), sticky="nw")
            self.btn_set_save_scale.grid_configure(row=0, column=10, padx=(0,6), pady=6, sticky="w")
            self.btn_info_scale.grid_configure(row=0, column=11, padx=(0,10), pady=(0,6), sticky="nw")
            self.btn_load_scale_file.grid_configure(row=0, column=12, padx=(0,6), pady=6, sticky="w")

    # ==========================
    # UI layout
    # ==========================
    def _build_ui(self, parent):
        self._build_header(parent)
        self._add_intro_text(
            parent,
            "This graphical user interface (GUI) controls the TARTAN device, "
            "developed to enable controlled, repeatable studies of contact-based transfer.\n"
            "The typical workflow is as follows:\n"
            "  1) Connect to both Arduinos and start the live data stream.\n"
            "  2) Establish the firmware tare offset and scale factor using zero load and a known reference mass.\n"
            "  3) Perform a multi-mass calibration check when required.\n"
            "  4) Run controlled transfer movements while recording data to file.\n\n"
            "This GUI and device are described in the following publications:\n"
            "TARTAN: a transparent, calibratable fibre transfer platform based on repurposed motion systems, Virginie Galais, Cameron Anderson, Niamh Nic Daéid, Hervé Ménard. "
            "Leverhulme Research Centre for Forensic Science, Department of Science and Engineering, "
            "University of Dundee, Dundee, DD1 4HN, UK.\n\n"
            "Users are free to reproduce, adapt, and build upon this work. "
            "When using the GUI and/or device in research or teaching, the above authors "
            "should be appropriately cited.\n\n"
            "For questions, issues, or collaboration enquiries, please contact: vgalais001@dundee.ac.uk"
        )

        s = ttk.LabelFrame(parent, text="")
        s.pack(fill="x", padx=(8,12), pady=(6,8))
        self._add_highlight(s, "1) Connection and getting the system ready")
        self._add_section_description(s,
            "Connect both Arduinos: the Uno controls the motors; the Nano reads the load cell. "
            "Both use their own USB cable and share no wiring. "
            "Start live data to begin receiving force values from the Nano.")
        self._build_connection(s)

        s = ttk.LabelFrame(parent, text="")
        s.pack(fill="x", padx=(8,12), pady=(0,8))
        self._add_highlight(s, "2) Firmware tare and scale-factor setup")
        self._add_section_description(s,
            "Establish the zero-load tare offset and the scale factor used to convert raw HX711 counts to grams. "
            "Capture the zero-load reference, then a known reference mass, and compute the scale factor. "
            "The scale factor is saved to the Nano EEPROM and can be reused across sessions, "
            "but the tare must be re-established at the start of each session.")
        self._build_tare(s)

        s = ttk.LabelFrame(parent, text="")
        s.pack(fill="both", expand=True, padx=12, pady=(0,8))
        self._add_highlight(s, "3) Multi-mass calibration check ")
        self._add_section_description(s,
            "Record repeated converted load-cell readings at several reference masses to assess residual measurement error. "
            "For transfer experiments, perform this check immediately before and after the experimental runs. "
            "This check does not modify the firmware scale factor.")
        self._build_calibration_recording(s)

        s = ttk.LabelFrame(parent, text="")
        s.pack(fill="x", padx=(8,12), pady=(0,8))
        self._add_highlight(s, "4) Sample run")
        self._add_section_description(s,
            "Run a controlled transfer movement while recording force and position data to a CSV file.")
        self._build_sample_run(s)

        self._set_controls_enabled(False)

    def _add_highlight(self, parent, text):
        bar = tk.Frame(parent, bg=HILITE_BG)
        bar.pack(fill="x", padx=8, pady=(8,8))
        tk.Label(bar, text=text, bg=HILITE_BG, fg=HILITE_FG,
                 font=("Segoe UI", 11, "bold")).pack(anchor="w", padx=8, pady=6)

    def _add_section_description(self, parent, text):
        lbl = ttk.Label(parent, text=text, justify="left")
        lbl.pack(fill="x", padx=12, pady=(0,8))
        parent.bind("<Configure>", lambda e: lbl.configure(wraplength=max(400, e.width-40)))

    def _add_intro_text(self, parent, text):
        frm = ttk.Frame(parent)
        frm.pack(fill="x", padx=(12,12), pady=(6,10))
        lbl = ttk.Label(frm, text=text, justify="left")
        lbl.pack(fill="x", anchor="w")
        frm.bind("<Configure>", lambda e: lbl.configure(wraplength=max(400, e.width-40)))

    def _show_info(self, title, text):
        win = tk.Toplevel(self)
        win.title(title)
        win.transient(self)
        win.attributes("-topmost", True)
        win.resizable(False, False)
        win.geometry(f"+{self.winfo_pointerx()+10}+{self.winfo_pointery()+10}")
        frm = tk.Frame(win, padx=12, pady=10)
        frm.pack(fill="both", expand=True)
        tk.Label(frm, text=text, justify="left", wraplength=340).pack(anchor="w")
        tk.Button(frm, text="OK", command=win.destroy).pack(anchor="e", pady=(10,0))
        win.update_idletasks()
        win.grab_set()
        win.focus_force()

    def _build_header(self, parent):
        header = ttk.Frame(parent)
        header.pack(fill="x", padx=(8,12), pady=(10,6))
        self.lrcfs_img   = self._load_logo(LRCFS_LOGO_CANDIDATES,      height=100)
        self.lev_img     = self._load_logo(LEVERHULME_LOGO_CANDIDATES,  height=100)
        self.tartan_logo = self._load_logo(["TARTAN_logo.png"],         height=150)
        ttk.Label(header, image=self.lrcfs_img).grid(row=0, column=0, sticky="w")
        mid = ttk.Frame(header)
        mid.grid(row=0, column=1, padx=15, sticky="n")
        ttk.Label(mid, image=self.tartan_logo).pack(pady=(0,4))
        ttk.Label(header, image=self.lev_img).grid(row=0, column=2, sticky="e")
        header.columnconfigure(1, weight=1)

    def _load_logo(self, candidates, height=55):
        for fn in candidates:
            path = ASSET_DIR / fn
            try:
                with Image.open(path) as source:
                    img = source.convert("RGBA")
                w, h = img.size
                img = img.resize((max(1, round(w * height / h)), height), Image.LANCZOS)
                print(f"[TARTAN images] Loaded: {path}")
                return ImageTk.PhotoImage(img)
            except (OSError, ValueError) as exc:
                print(f"[TARTAN images] Could not load {path}: {exc}")
        print(f"[TARTAN images] No logo found among: {candidates}")
        return tk.PhotoImage(width=1, height=1)

    def _load_small_icon(self, filename, size=16):
        path = ASSET_DIR / filename
        try:
            with Image.open(path) as source:
                img = source.convert("RGBA")
            img = img.resize((size, size), Image.LANCZOS)
            return ImageTk.PhotoImage(img)
        except (OSError, ValueError) as exc:
            print(f"[TARTAN images] Could not load icon {path}: {exc}")
            return None

    # ---------------- Section 1: Connection ----------------
    def _build_connection(self, parent):
        # ---- Uno (motors) ----
        row = ttk.Frame(parent)
        row.pack(fill="x", padx=(8,12), pady=(0,4))

        ttk.Label(row, text="Uno port (motors):").grid(row=0, column=0, padx=(0,6), pady=6, sticky="e")
        self.uno_port_var = tk.StringVar(value=DEFAULT_PORT_UNO)
        ttk.Entry(row, textvariable=self.uno_port_var, width=10).grid(row=0, column=1, padx=(0,2), pady=6)
        ttk.Button(row, image=self.info_icon, style="Info.TButton",
                   command=lambda: self._show_info("Uno port",
                       "COM port for the Arduino Uno (motors + limit switchs).\n"
                       "Found in Device Manager → Ports (COM & LPT).")
        ).grid(row=0, column=2, padx=(0,10), pady=(0,6), sticky="nw")

        ttk.Label(row, text="Baud:").grid(row=0, column=3, padx=(0,6), pady=6, sticky="e")
        self.uno_baud_var = tk.IntVar(value=DEFAULT_BAUD)
        ttk.Entry(row, textvariable=self.uno_baud_var, width=10).grid(row=0, column=4, padx=(0,2), pady=6)

        self.btn_connect_uno = ttk.Button(row, text="Connect Uno", command=self.on_connect_uno)
        self.btn_connect_uno.grid(row=0, column=5, padx=6, pady=6)

        self.btn_disconnect_uno = ttk.Button(row, text="Disconnect Uno",
                                             command=self.on_disconnect_uno, state="disabled")
        self.btn_disconnect_uno.grid(row=0, column=6, padx=6, pady=6)

        self.uno_status_text = tk.StringVar(value="Uno: Disconnected")
        self.uno_lbl_status  = tk.Label(row, textvariable=self.uno_status_text, fg="red")
        self.uno_lbl_status.grid(row=0, column=7, padx=10, pady=6, sticky="w")
        row.columnconfigure(8, weight=1)

        # ---- Nano (load cell) ----
        row2 = ttk.Frame(parent)
        row2.pack(fill="x", padx=(8,12), pady=(0,10))

        ttk.Label(row2, text="Nano port (load cell):").grid(row=0, column=0, padx=(0,6), pady=6, sticky="e")
        self.nano_port_var = tk.StringVar(value=DEFAULT_PORT_NANO)
        ttk.Entry(row2, textvariable=self.nano_port_var, width=10).grid(row=0, column=1, padx=(0,2), pady=6)
        ttk.Button(row2, image=self.info_icon, style="Info.TButton",
                   command=lambda: self._show_info("Nano port",
                       "COM port for the Arduino Nano (HX711 load cell only).\n"
                       "Found in Device Manager → Ports (COM & LPT).\n"
                       "Must be different from the Uno port.")
        ).grid(row=0, column=2, padx=(0,10), pady=(0,6), sticky="nw")

        ttk.Label(row2, text="Baud:").grid(row=0, column=3, padx=(0,6), pady=6, sticky="e")
        self.nano_baud_var = tk.IntVar(value=DEFAULT_BAUD)
        ttk.Entry(row2, textvariable=self.nano_baud_var, width=10).grid(row=0, column=4, padx=(0,2), pady=6)

        self.btn_connect_nano = ttk.Button(row2, text="Connect Nano", command=self.on_connect_nano)
        self.btn_connect_nano.grid(row=0, column=5, padx=6, pady=6)

        self.btn_disconnect_nano = ttk.Button(row2, text="Disconnect Nano",
                                              command=self.on_disconnect_nano, state="disabled")
        self.btn_disconnect_nano.grid(row=0, column=6, padx=6, pady=6)

        self.nano_status_text = tk.StringVar(value="Nano: Disconnected")
        self.nano_lbl_status  = tk.Label(row2, textvariable=self.nano_status_text, fg="red")
        self.nano_lbl_status.grid(row=0, column=7, padx=10, pady=6, sticky="w")

        # Start / stop live data (Nano stream only)
        self.btn_stream_on = ttk.Button(row2, text="Start live data",
                                        command=self._start_nano_stream, state="disabled")
        self.btn_stream_on.grid(row=0, column=8, padx=6, pady=6)

        self.btn_stream_off = ttk.Button(row2, text="Stop live data",
                                         command=self._stop_nano_stream, state="disabled")
        self.btn_stream_off.grid(row=0, column=9, padx=6, pady=6)

        row2.columnconfigure(10, weight=1)

    # ---------------- Section 2: Tare ----------------
    def _build_tare(self, parent):
        row = ttk.Frame(parent)
        row.pack(fill="x", padx=(4,12), pady=(0,6))
        self.tare_row = row

        ttk.Label(row, text="Number of samples to average (n)").grid(
            row=0, column=0, padx=(0,6), pady=6, sticky="e")
        self.raw_n_var = tk.StringVar(value=str(RAW_N_DEFAULT))
        ttk.Entry(row, textvariable=self.raw_n_var, width=8).grid(
            row=0, column=1, padx=(0,2), pady=6, sticky="w")
        ttk.Button(row, image=self.info_icon, style="Info.TButton",
                   command=lambda: self._show_info("Samples to average",
                       "Number of raw readings averaged per capture. "
                       "Typical values: 10–30. Does not affect live streaming.")
        ).grid(row=0, column=2, padx=(0,10), pady=(0,6), sticky="nw")

        ttk.Label(row, text="Reference mass (g):").grid(
            row=0, column=3, padx=(0,6), pady=6, sticky="e")
        self.known_weight_g_var = tk.StringVar(value="899.45")
        ttk.Entry(row, textvariable=self.known_weight_g_var, width=10).grid(
            row=0, column=4, padx=(0,2), pady=6, sticky="w")
        ttk.Button(row, image=self.info_icon, style="Info.TButton",
                   command=lambda: self._show_info("Reference mass (g)",
                       "Known reference mass used with the zero-load reading to establish the firmware scale factor. "
                       "Use an accurately measured mass within the load-cell operating range.")
        ).grid(row=0, column=5, padx=(0,10), pady=(0,6), sticky="nw")

        self.btn_raw0 = ttk.Button(row, text="Capture zero-load reference",
                                   command=self.on_capture_raw0, state="disabled")
        self.btn_raw0.grid(row=0, column=6, padx=(0,6), pady=6)
        self.btn_info_raw0 = ttk.Button(row, image=self.info_icon, style="Info.TButton",
            command=lambda: self._show_info("Capture zero-load reference",
                "Remove ALL load from the load cell, then click.\n"
                "The averaged raw HX711 output is used to establish the tare offset."))
        self.btn_info_raw0.grid(row=0, column=7, padx=(0,10), pady=(0,6), sticky="nw")

        self.btn_raw1 = ttk.Button(row, text="Capture reference mass",
                                   command=self.on_capture_raw1, state="disabled")
        self.btn_raw1.grid(row=0, column=8, padx=(0,6), pady=6)
        self.btn_info_raw1 = ttk.Button(row, image=self.info_icon, style="Info.TButton",
            command=lambda: self._show_info("Capture reference mass",
                "Place the reference mass on the load cell, then click.\n"
                "The averaged raw HX711 output is used with the zero-load reference to calculate the scale factor."))
        self.btn_info_raw1.grid(row=0, column=9, padx=(0,10), pady=(0,6), sticky="nw")

        self.btn_set_save_scale = ttk.Button(row, text="Compute + save scale factor",
                                             command=self.on_set_and_save_scale, state="disabled")
        self.btn_set_save_scale.grid(row=0, column=10, padx=(0,6), pady=6)
        self.btn_info_scale = ttk.Button(row, image=self.info_icon, style="Info.TButton",
            command=lambda: self._show_info("Compute + save scale factor",
                "scale factor = (raw_reference − raw_zero) / reference_mass_g\n\n"
                "The scale factor converts raw counts to grams and is saved to Nano EEPROM. "
                "A tare is then applied to re-establish the zero reference for the current session."))
        self.btn_info_scale.grid(row=0, column=11, padx=(0,10), pady=(0,6), sticky="nw")

        self.btn_load_scale_file = ttk.Button(row, text="Load scale factor from file",
                                              command=self.on_load_scale_from_file, state="disabled")
        self.btn_load_scale_file.grid(row=0, column=12, padx=(0,6), pady=6)

        row.columnconfigure(13, weight=1)
        row.bind("<Configure>", self._layout_tare_responsive)
        self.after(0, self._layout_tare_responsive)

        lift_row = ttk.Frame(parent)
        lift_row.pack(fill="x", padx=(8,12), pady=(0,4))
        self.btn_lift_platform = ttk.Button(lift_row, text="Lift platform (10 cm UP)",
                                            command=self.on_lift_platform, state="disabled")
        self.btn_lift_platform.grid(row=0, column=0, padx=(0,6), pady=6, sticky="w")
        ttk.Label(lift_row,
                  text="Moves platform 10 cm UP at 1 cm/s to allow weights to be placed on the load cell.",
                  foreground="gray").grid(row=0, column=1, padx=6, pady=6, sticky="w")

        # Calibration messages: red text, with selected instructions in red bold.
        self.tare_message = tk.Text(
            parent, height=2, wrap="word", font=("Segoe UI", 10),
            fg="red", bg="white", relief="flat", borderwidth=0,
            highlightthickness=0, cursor="arrow", takefocus=0
        )
        self.tare_message.pack(fill="x", padx=(8,12), pady=(0,8))
        self.tare_message.tag_configure(
            "red_bold", foreground="red", font=("Segoe UI", 10, "bold")
        )
        # Preserve all existing tare_status.set(...) calls in the GUI.
        self.tare_status.trace_add("write", self._update_tare_message)
        self._update_tare_message()

    def _update_tare_message(self, *_):
        """Display the latest calibration status and emphasise key instructions."""
        message = self.tare_status.get()
        self.tare_message.configure(state="normal")
        self.tare_message.delete("1.0", "end")
        self.tare_message.insert("1.0", message)

        emphasis = "Remove the reference weight"
        start = message.find(emphasis)
        if start >= 0:
            self.tare_message.tag_add(
                "red_bold", f"1.0+{start}c", f"1.0+{start + len(emphasis)}c"
            )
        self.tare_message.configure(state="disabled")

    # ---------------- Section 3: Multi-mass calibration check ----------------
    def _build_calibration_recording(self, parent):
        inputs = ttk.Frame(parent)
        inputs.pack(fill="x", padx=(8,12), pady=(0,4))

        ttk.Label(inputs, text="Reference masses (g):").grid(row=0, column=0, padx=6, pady=6, sticky="e")
        ttk.Button(inputs, image=self.info_icon, style="Info.TButton",
            command=lambda: self._show_info("Reference masses",
                "Enter the reference masses to be checked, separated by commas.\n"
                "Use accurately measured masses spanning the operating range. For an independent session check, "
                "do not use the reference mass used to establish the firmware scale factor.")
        ).grid(row=0, column=1, padx=(0,6), pady=(0,6), sticky="w")
        self.cal_weights_var = tk.StringVar(value="0,99.47,499.47,899.45")
        ttk.Entry(inputs, textvariable=self.cal_weights_var, width=40).grid(
            row=0, column=2, padx=6, pady=6, sticky="w")

        ttk.Label(inputs, text="n:").grid(row=0, column=3, padx=6, pady=6, sticky="e")
        self.cal_n_var = tk.StringVar(value="20")
        ttk.Entry(inputs, textvariable=self.cal_n_var, width=8).grid(
            row=0, column=4, padx=6, pady=6, sticky="w")

        ttk.Label(inputs, text="File label (optional):").grid(row=0, column=5, padx=6, pady=6, sticky="e")
        self.cal_label_var = tk.StringVar(value="LoadCell")
        ttk.Entry(inputs, textvariable=self.cal_label_var, width=18).grid(
            row=0, column=6, padx=6, pady=6, sticky="w")
        inputs.columnconfigure(7, weight=1)

        buttons = ttk.Frame(parent)
        buttons.pack(fill="x", padx=(8,12), pady=(0,6))
        self.btn_start_cal = ttk.Button(buttons, text="Start calibration check",
                                        command=self.on_start_calibration_recording, state="disabled")
        self.btn_start_cal.grid(row=0, column=0, padx=6, pady=6)
        self.btn_clear_cal = ttk.Button(buttons, text="Clear table",
                                        command=self.on_clear_cal_table, state="disabled")
        self.btn_clear_cal.grid(row=0, column=1, padx=6, pady=6)
        self.btn_save_cal_raw = ttk.Button(buttons, text="Save calibration-check data",
                                           command=self.on_save_cal_raw_file, state="disabled")
        self.btn_save_cal_raw.grid(row=0, column=2, padx=6, pady=6)
        buttons.columnconfigure(4, weight=1)

        table_holder = ttk.Frame(parent)
        table_holder.pack(fill="both", expand=True, padx=12, pady=(0,6))
        cols = ("Reference mass (g)", "Mean (g)", "Error (g)", "SD (g)", "n")
        self.cal_tree = ttk.Treeview(table_holder, columns=cols, show="headings", height=6)
        for c in cols:
            self.cal_tree.heading(c, text=c)
            self.cal_tree.column(c, width=160, anchor="center")
        vs = ttk.Scrollbar(table_holder, orient="vertical", command=self.cal_tree.yview)
        self.cal_tree.configure(yscrollcommand=vs.set)
        self.cal_tree.pack(side="left", fill="both", expand=True)
        vs.pack(side="right", fill="y")
        ttk.Label(parent, textvariable=self.cal_status).pack(fill="x", padx=(8,12), pady=(0,8))

    # ---------------- Section 4: Sample run ----------------
    def _build_sample_run(self, parent):
        row1 = ttk.Frame(parent)
        row1.pack(fill="x", padx=(8,12), pady=(0,4))
        ttk.Label(row1, text="Speed (cm/s):").grid(row=0, column=0, padx=6, pady=6, sticky="e")
        self.run_speed_cm_s = tk.StringVar(value="1.0")
        ttk.Entry(row1, textvariable=self.run_speed_cm_s, width=10).grid(row=0, column=1, padx=6, pady=6)
        ttk.Label(row1, text="Distance (cm):").grid(row=0, column=2, padx=6, pady=6, sticky="e")
        self.run_dist_cm = tk.StringVar(value="10.0")
        ttk.Entry(row1, textvariable=self.run_dist_cm, width=10).grid(row=0, column=3, padx=6, pady=6)
        ttk.Label(row1, text="Direction:").grid(row=0, column=4, padx=6, pady=6, sticky="e")
        self.run_dir = tk.StringVar(value="UP")
        ttk.Combobox(row1, textvariable=self.run_dir, values=["UP","DOWN"],
                     width=8, state="readonly").grid(row=0, column=5, padx=6, pady=6)
        row1.columnconfigure(6, weight=1)

        row2 = ttk.Frame(parent)
        row2.pack(fill="x", padx=(8,12), pady=(0,6))
        ttk.Label(row2, text="Run name:").grid(row=0, column=0, padx=6, pady=6, sticky="e")
        ttk.Entry(row2, textvariable=self.run_base_name, width=30).grid(row=0, column=1, padx=6, pady=6)
        ttk.Button(row2, text="Choose save location…",
                   command=self.on_choose_save_location).grid(row=0, column=2, padx=6, pady=6)
        self.save_loc_status = tk.StringVar(value="No save location selected.")
        ttk.Label(row2, textvariable=self.save_loc_status).grid(row=0, column=3, padx=6, pady=6, sticky="w")
        row2.columnconfigure(4, weight=1)

        row3 = ttk.Frame(parent)
        row3.pack(fill="x", padx=(8,12), pady=(0,4))
        self.btn_run = ttk.Button(row3, text="Run sample", command=self.on_run_sample, state="disabled")
        self.btn_run.grid(row=0, column=0, padx=6, pady=6)
        self.btn_stop = ttk.Button(row3, text="Stop", command=self.on_stop_sample, state="disabled")
        self.btn_stop.grid(row=0, column=1, padx=6, pady=6)
        self.btn_return_home = ttk.Button(row3, text="Return home",
                                          command=self.on_return_home, state="disabled")
        self.btn_return_home.grid(row=0, column=2, padx=6, pady=6)
        ttk.Label(row3, text="Return home speed (cm/s):").grid(row=0, column=3, padx=6, pady=6, sticky="e")
        self.home_speed_cm_s = tk.StringVar(value="1.0")
        ttk.Entry(row3, textvariable=self.home_speed_cm_s, width=10).grid(row=0, column=4, padx=6, pady=6)
        row3.columnconfigure(5, weight=1)

        ttk.Label(parent, textvariable=self.run_status).pack(fill="x", padx=(8,12), pady=(0,8))

    # ---------------- Live strip ----------------
    def _build_live_strip(self, parent):
        row = ttk.Frame(parent)
        row.pack(fill="x", padx=(8,12), pady=(0,10))
        self.v_force = tk.StringVar(value="—")
        self.v_mode  = tk.StringVar(value="—")
        ttk.Label(row, text="Load (g):").grid(row=0, column=0, padx=6, pady=6, sticky="e")
        ttk.Label(row, textvariable=self.v_force, width=12).grid(row=0, column=1, padx=6, pady=6)
        ttk.Label(row, text="State:").grid(row=0, column=2, padx=6, pady=6, sticky="e")
        ttk.Label(row, textvariable=self.v_mode,  width=10).grid(row=0, column=3, padx=6, pady=6)
        row.columnconfigure(4, weight=1)

    # ---------------- enable/disable ----------------
    def _set_controls_enabled(self, enabled: bool):
        state = "normal" if enabled else "disabled"
        for b in (self.btn_stream_on, self.btn_stream_off,
                  self.btn_raw0, self.btn_raw1,
                  self.btn_set_save_scale, self.btn_load_scale_file,
                  self.btn_lift_platform,
                  self.btn_start_cal, self.btn_clear_cal,
                  self.btn_save_cal_raw,
                  self.btn_run, self.btn_return_home):
            b.configure(state=state)

    # ==========================
    # Connection — Uno
    # ==========================
    def on_connect_uno(self):
        if self.uno_worker: return
        self.uno_worker = SerialWorker(
            self.uno_port_var.get().strip(), int(self.uno_baud_var.get()), self.uno_q)
        self.uno_worker.start()
        self.btn_connect_uno.configure(state="disabled")
        self.btn_disconnect_uno.configure(state="normal")

    def on_disconnect_uno(self):
        try:
            self.send_uno("OFF")
        except: pass
        if self.uno_worker:
            self.uno_worker.stop()
            self.uno_worker = None
        self.btn_connect_uno.configure(state="normal")
        self.btn_disconnect_uno.configure(state="disabled")
        self.uno_status_text.set("Uno: Disconnected")
        self.uno_lbl_status.configure(fg="red")
        if not self.nano_worker:
            self._set_controls_enabled(False)

    def send_uno(self, cmd: str):
        if self.uno_worker: self.uno_worker.send(cmd)

    # ==========================
    # Connection — Nano
    # ==========================
    def on_connect_nano(self):
        if self.nano_worker: return
        self.nano_worker = SerialWorker(
            self.nano_port_var.get().strip(), int(self.nano_baud_var.get()), self.nano_q)
        self.nano_worker.start()
        self.btn_connect_nano.configure(state="disabled")
        self.btn_disconnect_nano.configure(state="normal")

    def on_disconnect_nano(self):
        try:
            self.send_nano("STREAM_OFF")
        except: pass
        if self.nano_worker:
            self.nano_worker.stop()
            self.nano_worker = None
        self.btn_connect_nano.configure(state="normal")
        self.btn_disconnect_nano.configure(state="disabled")
        self.nano_status_text.set("Nano: Disconnected")
        self.nano_lbl_status.configure(fg="red")
        if not self.uno_worker:
            self._set_controls_enabled(False)

    def send_nano(self, cmd: str):
        if self.nano_worker: self.nano_worker.send(cmd)

    def _start_nano_stream(self):
        self.send_nano("STREAM_ON")

    def _stop_nano_stream(self):
        self.send_nano("STREAM_OFF")

    # ==========================
    # Serial polling
    # ==========================
    def _poll_serial(self):
        # Uno queue
        try:
            while True:
                kind, payload = self.uno_q.get_nowait()
                try:
                    if kind == "status":
                        if payload == "connected":
                            self.uno_status_text.set("Uno: Connected ✓")
                            self.uno_lbl_status.configure(fg="green")
                            self._set_controls_enabled(True)
                        else:
                            self.uno_status_text.set("Uno: Disconnected")
                            self.uno_lbl_status.configure(fg="red")
                            if not self.nano_worker:
                                self._set_controls_enabled(False)
                    elif kind == "status_msg":
                        if "ERROR" in payload or "error" in payload.lower():
                            messagebox.showerror("Uno serial", payload)
                    elif kind == "line":
                        self._handle_uno_line(payload)
                except Exception as e:
                    print(f"[poll_serial Uno] {e}")
        except queue.Empty: pass

        # Nano queue
        try:
            while True:
                kind, payload = self.nano_q.get_nowait()
                try:
                    if kind == "status":
                        if payload == "connected":
                            self.nano_status_text.set("Nano: Connected ✓")
                            self.nano_lbl_status.configure(fg="green")
                            self._set_controls_enabled(True)
                            self.after(1200, self._start_nano_stream)
                        else:
                            self.nano_status_text.set("Nano: Disconnected")
                            self.nano_lbl_status.configure(fg="red")
                            if not self.uno_worker:
                                self._set_controls_enabled(False)
                    elif kind == "status_msg":
                        if "ERROR" in payload or "error" in payload.lower():
                            messagebox.showerror("Nano serial", payload)
                    elif kind == "line":
                        self._handle_nano_line(payload)
                except Exception as e:
                    print(f"[poll_serial Nano] {e}")
        except queue.Empty: pass

        self.after(50, self._poll_serial)

    # ==========================
    # Uno line handler — events only
    # ==========================
    def _handle_uno_line(self, s: str):
        if not s or s.startswith("#"): return
        print("UNO RX <", s)

        if s.startswith("STEP_COUNT,"):
            try:
                count = int(s.split(",", 1)[1])
            except (ValueError, IndexError):
                return
            if self.logging and self.run_active and self.uno_move_started and count >= 0:
                self.uno_issued_steps = count
            return

        if not s.startswith("EVENT,"): return

        parts = s.split(",")
        event = parts[1] if len(parts) > 1 else ""

        if event == "MOVE_START":
            if self.run_active:
                self.uno_move_started = True
                self.uno_issued_steps = 0
            self.live["mode"] = "MOVE"
            self.v_mode.set("MOVE")
            if self.run_active:
                self.run_started_move = True
                self.run_moving = True
                # Step tracking is already set by do_move() before the MOVE command
                # is sent — do not overwrite it here.

        elif event == "MOVE_DONE":
            if self.run_active and self.logging:
                self.run_completion_received = True
                # The Uno already sent its final STEP_COUNT before MOVE_DONE.
                # Do not add a synthetic CSV row with an empty force value.
                # Nano readings during the short post-move window carry the final count.
            self.live["mode"] = "IDLE"
            self.v_mode.set("IDLE")
            self.run_moving = False
            if self.run_active:
                # Clear run_active immediately so Return Home is never blocked.
                # Defer file close by 200 ms to catch any in-flight Nano samples.
                self.run_active = False
                self.run_started_move = False
                self.after(400, self._finish_run_logging)
            if self.hh_cal_active and self.hh_cal_stage == "MOVE_OUT":
                self.hh_cal_stage = "HOMING"
                self.run_status.set("Platform position calibration: homing DOWN to limit switch...")
                self.send_uno("HOME_HIT 1.0")

        elif event == "HOME_START":
            self.live["mode"] = "HOME"
            self.v_mode.set("HOME")

        elif event == "HOME_DONE":
            self.live["mode"] = "IDLE"
            self.v_mode.set("IDLE")
            if self.hh_cal_active and self.hh_cal_stage == "HOMING":
                self.hh_cal_active = False
                self.hh_cal_stage  = None
                self.run_status.set("Platform position calibration: done.")

        elif event == "HOME_TIMEOUT":
            self.live["mode"] = "IDLE"
            self.v_mode.set("IDLE")
            messagebox.showwarning("Home timeout", "Home move timed out — limit switch not reached.")

        elif event == "BOTTOM_LIMIT":
            if self.run_active:
                self._end_aborted_run("Bottom limit switch hit — incomplete run.")
            self.live["mode"] = "IDLE"
            self.v_mode.set("E-STOP")
            messagebox.showwarning("Bottom limit", "Downward movement stopped at bottom endstop.")

        elif event == "ESTOP":
            if self.run_active:
                self._end_aborted_run("Top limit switch hit — incomplete run.")
            # Top limit switch hit during an upward move. The Uno has already stopped
            # the motors, so we only need to tear down any active run and warn.
            self.live["mode"] = "IDLE"
            self.v_mode.set("E-STOP")

            # Cancel any pending / auto-finish timers from an active run.
            if getattr(self, "_auto_finish_id", None) is not None:
                try: self.after_cancel(self._auto_finish_id)
                except: pass
                self._auto_finish_id = None
            if self.move_after_id is not None:
                try: self.after_cancel(self.move_after_id)
                except: pass
                self.move_after_id = None

            # Close any open log file cleanly (motors are already off).
            self.logging          = False
            self.run_active       = False
            self.run_started_move = False
            self.run_moving       = False
            self.run_total_steps     = 0
            self.run_move_start_time = None
            try:
                if self.log_fp:
                    self.log_fp.flush()
                    self.log_fp.close()
            except: pass
            self.log_fp     = None
            self.log_writer = None

            # Abort any in-progress platform-position calibration.
            self.hh_cal_active = False
            self.hh_cal_stage  = None

            try:
                self.btn_stop.configure(state="disabled")
                self.btn_run.configure(state="normal")
            except: pass

            self.run_status.set("EMERGENCY STOP: top limit switch triggered — motors stopped.")
            messagebox.showwarning(
                "Emergency stop",
                "The top limit switch was triggered while moving up.\n"
                "The motors have been stopped.\n\n"
                "Use 'Return home'to bring the platform back down.")

        elif event == "OFF":
            self.live["mode"] = "IDLE"
            self.v_mode.set("IDLE")

    # ==========================
    # Nano line handler — force stream + scale responses
    # ==========================
    def _handle_nano_line(self, s: str):
        if not s or s.startswith("#") or s.startswith("t_ms,"): return
        print("NANO RX <", s)

        if s.startswith("RAW,"):
            try: self.latest_raw = int(s.split(",", 1)[1].strip())
            except: pass
            return

        if s.startswith("SCALE,"):
            try:
                v = float(s.split(",", 1)[1].strip())
                self.latest_scale = v
            except: return
            expected = getattr(self, "_pending_scale_expected", None)
            if expected is not None:
                if abs(v - expected) < 1e-3:
                    self.tare_status.set(f"✅ Scale factor confirmed on Nano: {v:.6f} counts/g")
                else:
                    self.tare_status.set(f"⚠️ Nano scale factor is {v:.6f}, expected {expected:.6f}")
                self._pending_scale_expected = None
            else:
                self.tare_status.set(f"Nano scale factor: {v:.6f} counts/g")
            return

        # 2-col stream: t_ms,force_g
        parts = s.split(",")
        if len(parts) != 2: return
        try:
            force_g = float(parts[1])
        except: return

        self.live["force_g"] = force_g
        self.nano_buffer.append((time.time(), force_g))
        self.v_force.set(f"{force_g:.2f}")

        # Write to CSV log if running
        if self.logging and self.log_writer:
            if self.run_completion_received:
                self.run_post_completion_samples += 1
            now      = time.localtime()
            speed_cs = getattr(self, "current_run_speed_cm_s", "")
            # Step count is issued by the Uno; no elapsed-time estimation.
            # A blank cell means no real pulse count has arrived yet.
            issued = self.uno_issued_steps
            distance_cm = (issued / (STEPS_PER_MM * 10.0)) if issued is not None else None
            self.log_writer.writerow({
                "date":       time.strftime("%Y-%m-%d", now),
                "time":       time.strftime("%H:%M:%S", now),
                "speed_cm_s": f"{float(speed_cs):.1f}" if speed_cs != "" else "",
                "steps":      issued if issued is not None else "",
                "distance_cm": f"{distance_cm:.3f}" if distance_cm is not None else "",
                "force_g":    f"{force_g:.3f}",
            })

    # ==========================
    # Section 2: Tare — all to Nano
    # ==========================
    def _get_raw_n(self) -> int:
        try: n = int(self.raw_n_var.get())
        except: n = RAW_N_DEFAULT
        return max(1, min(50, n))

    def _wait_for_raw(self, timeout_s=5.0):
        t0 = time.time()
        while time.time() - t0 < timeout_s:
            if self.latest_raw is not None: return self.latest_raw
            time.sleep(0.05)
        raise TimeoutError("No RAW response from Nano.")

    def on_capture_raw0(self):
        if not self.nano_worker:
            messagebox.showerror("Nano not connected", "Connect the Nano first.")
            return
        messagebox.showinfo("Zero-load reference", "Remove ALL load from the load cell.\nClick OK to capture the zero-load reference.")
        self.latest_raw = None
        self.send_nano(f"RAW {self._get_raw_n()}")
        def w():
            try:
                self.raw0 = self._wait_for_raw()
                self.after(0, lambda: self.tare_status.set(
                    f"Zero-load reference captured: raw={self.raw0}. Now place the reference mass and click 'Capture reference mass"
                ))
            except Exception as e:
                messagebox.showerror("Zero load", str(e))
        threading.Thread(target=w, daemon=True).start()

    def on_capture_raw1(self):
        if not self.nano_worker:
            messagebox.showerror("Nano not connected", "Connect the Nano first.")
            return
        if self.raw0 is None:
            messagebox.showwarning("Missing", "Capture zero load first.")
            return
        try:
            wgt = float(self.known_weight_g_var.get())
            if wgt <= 0: raise ValueError
        except:
            messagebox.showerror("Reference mass", "Enter a valid reference mass in grams.")
            return
        messagebox.showinfo("Reference mass", f"Place the reference mass ({wgt:.2f} g).\nClick OK to capture.")
        self.latest_raw = None
        self.send_nano(f"RAW {self._get_raw_n()}")
        def w():
            try:
                self.raw1 = self._wait_for_raw()
                diff = self.raw1 - self.raw0
                self.after(0, lambda: self.tare_status.set(
                    f"Reference mass captured ({wgt:.2f} g): raw={self.raw1}, diff={diff}.\n"
                    "Remove the reference weight, then click 'Compute + save scale factor'."
                ))
            except Exception as e:
                messagebox.showerror("Reference mass", str(e))
        threading.Thread(target=w, daemon=True).start()

    def on_set_and_save_scale(self):
        if self.raw0 is None or self.raw1 is None:
            messagebox.showwarning("Missing", "Capture the zero-load reference and reference mass first.")
            return
        try:
            wgt = float(self.known_weight_g_var.get())
            if wgt <= 0: raise ValueError
        except:
            messagebox.showerror("Reference mass", "Enter a valid reference mass in grams.")
            return
        diff = self.raw1 - self.raw0
        if abs(diff) < 10:
            messagebox.showerror("Scale factor", "Raw-count difference is too small — check the reference mass and wiring.")
            return
        scale_val = diff / wgt
        self.send_nano(f"SET_SCALE {scale_val:.6f}")
        time.sleep(0.15)
        self.send_nano("SAVE_SCALE")
        time.sleep(0.15)
        self.send_nano("TARE")
        self._pending_scale_expected = float(scale_val)
        self.send_nano("GET_SCALE")
        try:
            self._autosave_scale_file(scale_val, self.raw0, self.raw1, wgt)
        except Exception as e:
            messagebox.showwarning("Scale-factor file", f"Scale factor applied, but could not save file.\n\n{e}")
        self.tare_status.set(f"Scale factor set and saved on Nano: {scale_val:.6f} counts/g. Tare applied for this session.")

    def on_load_scale_from_file(self):
        if not self.nano_worker:
            messagebox.showerror("Nano not connected", "Connect the Nano first.")
            return
        path = filedialog.askopenfilename(
            title="Select a saved scale-factor file",
            filetypes=[("CSV files","*.csv"),("Text files","*.txt"),("All files","*.*")])
        if not path: return
        try:
            scale_val = self._read_scale_value_from_file(path)
        except Exception as e:
            messagebox.showerror("Load scale factor", str(e))
            return
        self._pending_scale_expected = float(scale_val)
        self.send_nano(f"SET_SCALE {scale_val:.6f}")
        do_save = messagebox.askyesno("Save scale factor to Nano?", "Save this scale factor to Nano EEPROM as well?")
        do_tare = messagebox.askyesno("Re-tare now?", "Re-establish the zero reference now?\n\nThis should be done at the start of each session with no load applied.")
        def _followup():
            if do_save: self.send_nano("SAVE_SCALE")
            if do_tare: self.send_nano("TARE")
            self.send_nano("GET_SCALE")
        self.after(150, _followup)

    def _get_scale_save_dir(self) -> str:
        d = os.path.join(os.path.dirname(os.path.abspath(__file__)), "SavedScales")
        os.makedirs(d, exist_ok=True)
        return d

    def _autosave_scale_file(self, scale_counts_per_g, raw0, raw1, known_weight_g) -> str:
        from datetime import datetime
        label     = getattr(self, "cal_label_var", None)
        label     = (label.get().strip() if label else "") or "TARTAN"
        safe      = "".join(c if c.isalnum() or c in "-_" else "_" for c in label)
        path      = os.path.join(self._get_scale_save_dir(),
                                 f"{datetime.now().strftime('%Y%m%d_%H%M%S')}_scale_{safe}.csv")
        with open(path, "w", newline="", encoding="utf-8") as fp:
            w = csv.DictWriter(fp, fieldnames=[
                "datetime","label","scale_counts_per_g","known_weight_g","raw0","raw1","diff_raw"])
            w.writeheader()
            w.writerow({
                "datetime":          datetime.now().isoformat(timespec="seconds"),
                "label":             label,
                "scale_counts_per_g":f"{scale_counts_per_g:.6f}",
                "known_weight_g":    f"{known_weight_g:.6f}",
                "raw0":              f"{raw0:.0f}",
                "raw1":              f"{raw1:.0f}",
                "diff_raw":          f"{raw1-raw0:.0f}",
            })
        return path

    def _read_scale_value_from_file(self, path: str) -> float:
        if path.lower().endswith(".csv"):
            with open(path, "r", encoding="utf-8-sig", newline="") as fp:
                rows = list(csv.DictReader(fp))
            if not rows: raise ValueError("CSV is empty.")
            for k in ("scale_counts_per_g","HX711_SCALE","hx711_scale","scale","scale_value"):
                if k in rows[0] and rows[0][k]:
                    return float(rows[0][k].strip())
            for v in rows[0].values():
                try: return float(str(v).strip())
                except: pass
            raise ValueError("No scale value column found.")
        with open(path, "r", encoding="utf-8-sig") as fp:
            txt = fp.read().strip()
        if not txt: raise ValueError("File is empty.")
        try: return float(txt)
        except: pass
        for tok in txt.replace(",", " ").split():
            try: return float(tok)
            except: pass
        raise ValueError("No numeric value found.")

    # ==========================
    # Section 3: Multi-mass calibration check
    # ==========================
    def on_clear_cal_table(self):
        for iid in self.cal_tree.get_children(): self.cal_tree.delete(iid)
        self.cal_results     = []
        self.cal_raw_results = []
        self.cal_status.set("Cleared.")

    def _parse_cal_weights(self):
        items = [x.strip() for x in self.cal_weights_var.get().split(",") if x.strip()]
        if not items: raise ValueError
        out = []
        for s in items:
            v   = float(s)
            dec = len(s.split(".",1)[1]) if "." in s else 0
            out.append((v, dec))
        return out

    def _parse_cal_n(self):
        try: n = int(self.cal_n_var.get())
        except: n = 20
        return max(3, min(500, n))

    def on_start_calibration_recording(self):
        if not self.nano_worker:
            messagebox.showerror("Nano not connected", "Connect the Nano first.")
            return
        if self.live["force_g"] is None:
            messagebox.showerror("No live data", "Start live data stream first.")
            return
        if self.cal_running: return
        try:
            self.cal_weights = self._parse_cal_weights()
            self.cal_n       = self._parse_cal_n()
        except:
            messagebox.showerror("Input error", "Enter weights like: 0,100,500,900")
            return
        self.cal_index   = 0
        self.cal_running = True
        self.on_clear_cal_table()
        self.cal_raw_results = []
        self.cal_status.set("Multi-mass calibration check started.")
        self._prompt_and_capture_next_weight()

    def _prompt_and_capture_next_weight(self):
        if not self.cal_running: return
        if self.cal_index >= len(self.cal_weights):
            self.cal_running = False
            self.cal_status.set("Finished. Click 'Save calibration-check data'.")
            messagebox.showinfo("Done", "All weights recorded.")
            return
        target_g, target_dec = self.cal_weights[self.cal_index]
        target_txt = f"{target_g:.{target_dec}f}"
        ok = messagebox.askokcancel("Next weight",
             f"Place: {target_txt} g\n\nClick OK to capture (n={self.cal_n}).")
        if not ok:
            self.cal_running = False
            self.cal_status.set("Stopped.")
            return
        samples = []
        def sampler():
            for _ in range(self.cal_n):
                if self.live["force_g"] is not None:
                    samples.append(float(self.live["force_g"]))
                time.sleep(0.12)
            self.after(0, lambda: self._finish_one_weight(target_g, target_dec, samples))
        threading.Thread(target=sampler, daemon=True).start()

    def _finish_one_weight(self, target_g, target_dec, samples):
        if len(samples) < 3:
            messagebox.showwarning("Capture", "Not enough samples. Check live data and retry.")
            return
        m      = mean(samples)
        sd     = sample_sd(samples)
        diff_g = m - target_g
        for i, v in enumerate(samples, 1):
            self.cal_raw_results.append({
                "date":         today_yyyymmdd(),
                "label":        self.cal_label_var.get().strip() or "LoadCell",
                "weight_index": self.cal_index,
                "target_g":     target_g,
                "sample_index": i,
                "force_g":      float(v),
            })
        target_txt = f"{target_g:.{target_dec}f}"
        self.cal_tree.insert("", "end", values=(
            target_txt, f"{m:.2f}", f"{diff_g:+.2f}", f"{sd:.2f}", str(len(samples))))
        self.cal_results.append({
            "date":target_txt,"target_g":target_g,"mean_g":m,"sd_g":sd,"diff_g":diff_g,"n":len(samples)})
        self.cal_status.set(
            f"Recorded {target_txt} g → Mean={m:.2f}, Error={diff_g:+.2f}, SD={sd:.2f} (n={len(samples)})")
        self.cal_index += 1
        self._prompt_and_capture_next_weight()

    def on_save_cal_file(self):
        rows = [self.cal_tree.item(iid,"values") for iid in self.cal_tree.get_children("")]
        if not rows: messagebox.showwarning("Empty","Table is empty."); return
        label = self.cal_label_var.get().strip() or "LoadCell"
        path  = filedialog.asksaveasfilename(
            initialfile=f"{today_yyyymmdd()}_CalibrationCheck_{label}.csv",
            defaultextension=".csv", filetypes=[("CSV","*.csv"),("All files","*.*")])
        if not path: return
        os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
        with open(path, "w", newline="", encoding="utf-8") as fp:
            w = csv.writer(fp)
            w.writerow(["Reference mass (g)","Mean (g)","Error (g)","SD (g)","n"])
            for r in rows: w.writerow(list(r))
        self.cal_status.set(f"Saved: {path}")
        messagebox.showinfo("Saved", f"Calibration check saved:\n{path}")

    def on_save_cal_raw_file(self):
        if not self.cal_raw_results:
            messagebox.showwarning("Empty","No raw data recorded yet."); return
        label = self.cal_label_var.get().strip() or "LoadCell"
        path  = filedialog.asksaveasfilename(
            initialfile=f"{today_yyyymmdd()}_CalibrationCheck_{label}.csv",
            defaultextension=".csv", filetypes=[("CSV","*.csv"),("All files","*.*")])
        if not path: return
        os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
        try:
            with open(path, "w", newline="", encoding="utf-8") as fp:
                w = csv.DictWriter(fp,
                    fieldnames=["date","label","weight_index","target_g","sample_index","force_g"],
                    extrasaction="ignore")
                w.writeheader()
                for r in self.cal_raw_results: w.writerow(r)
            self.cal_status.set(f"Saved calibration-check data: {path}")
            messagebox.showinfo("Saved", f"Calibration-check data saved:\n{path}")
        except Exception as e:
            messagebox.showerror("Save error", str(e))

    # ==========================
    # Section 4: Sample run
    # ==========================
    def on_choose_save_location(self):
        folder = filedialog.askdirectory(title="Select a folder to save run data")
        if not folder: return
        self.run_out_dir = folder
        if not self.run_base_name.get().strip():
            self.run_base_name.set("run")
        self.save_loc_status.set(f"Saving to: {folder}")

    def _sanitize(self, s: str) -> str:
        s = (s or "").strip()
        if not s: return "run"
        return "".join(c if c.isalnum() or c in "-_ " else "_" for c in s).strip()

    def _dedupe_path(self, path: str) -> str:
        if not os.path.exists(path): return path
        base, ext = os.path.splitext(path)
        i = 1
        while os.path.exists(f"{base} ({i}){ext}"): i += 1
        return f"{base} ({i}){ext}"

    def _make_run_filepath(self) -> str:
        if not self.run_out_dir: raise ValueError("No save location selected.")
        label    = self._sanitize(self.run_base_name.get())
        filename = f"{time.strftime('%Y%m%d_%H%M%S')}_{label}.csv"
        return self._dedupe_path(os.path.join(self.run_out_dir, filename))

    def on_lift_platform(self):
        if not self.uno_worker: return
        if self.run_active:
            messagebox.showwarning("Busy","A run is active."); return
        self.tare_status.set("Lifting platform 10 cm UP at 1 cm/s...")
        self.send_uno("MOVE 100.000 UP 1.0")

    def on_return_home(self):
        if not self.uno_worker: return
        if self.run_moving:
            messagebox.showwarning("Busy", "A move is still in progress — wait or click Stop first."); return
        try:
            sp = validate_speed(float(self.home_speed_cm_s.get()))
        except Exception as e:
            messagebox.showerror("Speed", str(e)); return
        self.run_status.set(f"Returning home at {sp:.1f} cm/s...")
        self.send_uno(f"HOME_HIT {sp:.1f}")

    def on_run_sample(self):
        if not self.uno_worker:
            messagebox.showerror("Uno not connected","Connect the Uno first."); return
        if self.logging or self.run_active: return
        try:
            speed_cm_s = validate_speed(float(self.run_speed_cm_s.get()))
            dist_cm    = float(self.run_dist_cm.get())
            if dist_cm < 0: raise ValueError
        except Exception as e:
            messagebox.showerror("Inputs", str(e)); return

        direction = self.run_dir.get().strip().upper()
        if direction not in ("UP","DOWN"): direction = "UP"
        dist_mm = cm_to_mm(dist_cm)

        if not self.run_out_dir:
            messagebox.showwarning("Save location","Choose a save location first."); return

        try:
            path = self._make_run_filepath()
        except Exception as e:
            messagebox.showerror("Save path", str(e)); return

        os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
        try:
            self.log_fp     = open(path, "w", newline="", encoding="utf-8")
            self.current_run_speed_cm_s = float(speed_cm_s)
            self.log_writer = csv.DictWriter(self.log_fp,
                fieldnames=["date","time","speed_cm_s","steps","distance_cm","force_g"])
            self.log_writer.writeheader()
            self.log_fp.flush()
        except Exception as e:
            messagebox.showerror("Log file", str(e)); return

        self.logging          = True
        self.run_active       = True
        self.run_started_move = False
        self.uno_issued_steps = 0
        self.uno_move_started = False
        self.run_completion_received = False
        self.run_post_completion_samples = 0
        self.btn_run.configure(state="disabled")
        self.btn_stop.configure(state="normal")

        # Ensure Nano is streaming
        self.send_nano("STREAM_ON")

        self.run_status.set(f"Logging to: {path}\nStarting move in {MOVE_START_DELAY_S:.1f}s...")

        if self.move_after_id is not None:
            try: self.after_cancel(self.move_after_id)
            except: pass

        def do_move():
            if self.logging and self.run_active:
                self.uno_issued_steps = 0
                self.uno_move_started = False
                self.run_completion_received = False
                self.send_uno(f"MOVE {dist_mm:.3f} {direction} {speed_cm_s:.1f}")
                self.run_moving          = True
                self.run_started_move    = True
                self.run_total_steps     = int(dist_mm * STEPS_PER_MM + 0.5)
                self.run_steps_per_sec   = speed_cm_s * 10.0 * STEPS_PER_MM
                self.run_move_start_time = time.time()
                self.run_status.set(
                    f"Moving: {dist_cm:.2f} cm {direction} at {speed_cm_s:.2f} cm/s\n"
                    f"Logging to: {path}")
                move_duration_ms = int((dist_mm / (speed_cm_s * 10.0)) * 1000) + 3000
                if getattr(self, "_auto_finish_id", None) is not None:
                    try: self.after_cancel(self._auto_finish_id)
                    except: pass
                self._auto_finish_id = self.after(move_duration_ms, self._auto_finish_if_still_running)

        self.move_after_id = self.after(int(MOVE_START_DELAY_S * 1000), do_move)

    def _end_aborted_run(self, reason):
        """Save the last real count; make any interrupted run visibly incomplete."""
        if getattr(self, "_auto_finish_id", None) is not None:
            try: self.after_cancel(self._auto_finish_id)
            except: pass
            self._auto_finish_id = None
        # No invented force row on abort: retain only genuine Nano samples.
        self.run_active = False
        self.run_moving = False
        self.run_started_move = False
        self._finish_run_logging()
        self.run_status.set(reason)

    def _finish_run_logging(self):
        """Close the log file after an auto-stop (MOVE_DONE).
        run_active is already False by this point.
        Guard against firing into a new run that started within the 200ms window."""
        if self.run_active:
            return  # a new run started before we could clean up — leave it alone
        self.logging          = False
        self.run_started_move = False
        self.run_moving       = False
        self.run_total_steps     = 0
        self.run_move_start_time = None
        try:
            if self.log_fp:
                self.log_fp.flush()
                self.log_fp.close()
        except: pass
        self.log_fp     = None
        self.log_writer = None
        self.btn_stop.configure(state="disabled")
        self.btn_run.configure(state="normal")
        if self.run_completion_received and not self.run_post_completion_samples:
            self.run_status.set("Run complete — saved. No post-completion Nano force sample received; the final recorded row may precede 10 cm.")
        else:
            self.run_status.set("Run complete — saved.")

    def _auto_finish_if_still_running(self):
        """Communication watchdog: stop Uno, mark run incomplete, never claim success."""
        self._auto_finish_id = None
        if self.run_active:
            self.send_uno("OFF")
            self._end_aborted_run("WARNING: MOVE_DONE not received. OFF sent; run incomplete. Check Uno and hardware.")
            messagebox.showwarning("Movement timeout", "No MOVE_DONE from Uno. OFF sent; saved data are incomplete. Verify motors are stopped.")

    def on_stop_sample(self):
        """Manual stop — abort motor and close log immediately."""
        if not (self.logging or self.run_active): return
        if getattr(self, "_auto_finish_id", None) is not None:
            try: self.after_cancel(self._auto_finish_id)
            except: pass
            self._auto_finish_id = None
        if self.move_after_id is not None:
            try: self.after_cancel(self.move_after_id)
            except: pass
            self.move_after_id = None
        try: self.send_uno("OFF")
        except: pass
        self.logging          = False
        self.run_active       = False
        self.run_started_move = False
        self.run_moving       = False
        self.run_total_steps     = 0
        self.run_move_start_time = None
        try:
            if self.log_fp:
                self.log_fp.flush()
                self.log_fp.close()
        except: pass
        self.log_fp     = None
        self.log_writer = None
        self.btn_stop.configure(state="disabled")
        self.btn_run.configure(state="normal")
        self.run_status.set("Stopped / saved.")

    def on_close(self):
        try: self.on_stop_sample()
        except: pass
        try: self.on_disconnect_uno()
        except: pass
        try: self.on_disconnect_nano()
        except: pass
        self.destroy()


if __name__ == "__main__":
    app = App()
    app.mainloop()
