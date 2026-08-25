// ============================================================
// TARTAN – Load Cell Acquisition Firmware
// Arduino Nano Every
//
// Interfaces with a strain-gauge load cell via an HX711 24-bit
// analogue-to-digital converter. Handles tare, manual scale
// factor entry, EEPROM persistence of the calibration factor,
// and continuous force streaming to the host computer at 10 Hz.
//
// Commandscan be sent directly from the Arduino IDE Serial Monitor
// (set to 115200 baud, Newline line ending) or from any
// serial terminal.
//
// Requires the HX711 library by Bogdan Necula / bogde:
//   https://github.com/bogde/HX711
//
// ── Wiring ────────────────────────────────────────────────
//   D7  – HX711 DT  (data)
//   D6  – HX711 SCK (clock)
//   5V  – HX711 VCC
//   GND – HX711 GND
//
// ── Calibration procedure ─────────────────────────────────
//   1. Send TARE with no load on the cell.
//   2. Send RAW 20 with no load — note the value (should be ~0).
//   3. Place a known mass on the load cell.
//   4. Send RAW 20 again — note the raw value with mass.
//   5. Compute: scale_factor = (raw_with_mass - raw_zero) / mass_g
//   6. Send SET_SCALE <scale_factor>
//   7. Verify with STREAM_ON — readings should match the known mass.
//   8. Send SAVE_SCALE to persist the factor to EEPROM.
//      The factor is reloaded automatically on next power-up.
//
// ── Serial interface ──────────────────────────────────────
//   Baud: 115200  |  Line ending: Newline (\n)
//
//   Commands:
//     TARE
//         Zero the load cell at the current reading.
//
//     RAW <n>
//         Read and print the average of <n> raw ADC counts
//         (no scale factor applied). Used during calibration.
//         e.g.  RAW 20
//
//     SET_SCALE <value>
//         Set the calibration scale factor manually.
//         e.g.  SET_SCALE -7050.0
//
//     GET_SCALE
//         Print the currently active scale factor.
//
//     SAVE_SCALE
//         Write the current scale factor to EEPROM so it
//         survives power cycling.
//
//     LOAD_SCALE
//         Reload the scale factor from EEPROM.
//
//     STREAM_ON
//         Begin streaming force readings at 10 Hz.
//         Output format: t_ms,force_g
//
//     STREAM_OFF
//         Stop streaming.
//
//   Responses prefixed with '#' are informational messages.
//   Responses without '#' are data lines intended for logging.
// ============================================================

#include <Arduino.h>
#include "HX711.h"
#include <EEPROM.h>

// ---------------- Pins ----------------
#define HX711_DT_PIN  2
#define HX711_SCK_PIN 3

// ---------------- Config ----------------
float HX711_SCALE = 1.0f;
const unsigned long STREAM_INTERVAL_MS = 100;   // 10 Hz

// EEPROM storage
const uint32_t EEPROM_MAGIC    = 0x54415254;
const int EEPROM_ADDR_MAGIC    = 0;
const int EEPROM_ADDR_SCALE    = EEPROM_ADDR_MAGIC + sizeof(uint32_t);

// ---------------- State ----------------
HX711 scale;
bool streamEnabled = false;
unsigned long lastStreamMs = 0;

// ---------------- EEPROM helpers ----------------
void saveScaleToEEPROM(float s) {
  EEPROM.put(EEPROM_ADDR_MAGIC, EEPROM_MAGIC);
  EEPROM.put(EEPROM_ADDR_SCALE, s);
  Serial.print("# SAVED scale to EEPROM: ");
  Serial.println(s, 6);
}

bool loadScaleFromEEPROM(float &outScale) {
  uint32_t magic = 0;
  EEPROM.get(EEPROM_ADDR_MAGIC, magic);
  if (magic != EEPROM_MAGIC) return false;
  float s = 0.0f;
  EEPROM.get(EEPROM_ADDR_SCALE, s);
  if (!isfinite(s) || fabs(s) < 1e-6) return false;
  outScale = s;
  return true;
}

// ---------------- Command actions ----------------
void doSetScale(float s) {
  HX711_SCALE = s;
  scale.set_scale(HX711_SCALE);
  Serial.print("SCALE,");
  Serial.println(HX711_SCALE, 6);
}

void doRawRead(int n) {
  if (n < 1) n = 1;
  if (n > 50) n = 50;
  long raw = scale.read_average(n);
  Serial.print("RAW,");
  Serial.println(raw);
}

// ---------------- Command parser ----------------
void handleCommand(String cmd) {
  cmd.trim();
  if (cmd.length() == 0) return;

  String upper = cmd;
  upper.toUpperCase();

  if (upper == "TARE") {
    scale.tare();
    Serial.println("# TARE done.");
    return;
  }

  if (upper == "STREAM_ON") {
    streamEnabled = true;
    lastStreamMs = 0;
    Serial.println("# STREAM ON");
    Serial.println("t_ms,force_g");
    return;
  }

  if (upper == "STREAM_OFF") {
    streamEnabled = false;
    Serial.println("# STREAM OFF");
    return;
  }

  if (upper.startsWith("RAW")) {
    int spc = cmd.indexOf(' ');
    int n = 10;
    if (spc > 0) n = cmd.substring(spc + 1).toInt();
    doRawRead(n);
    return;
  }

  if (upper.startsWith("SET_SCALE")) {
    int spc = cmd.indexOf(' ');
    if (spc < 0) { Serial.println("# Usage: SET_SCALE <value>"); return; }
    float s = cmd.substring(spc + 1).toFloat();
    doSetScale(s);
    return;
  }

  if (upper == "GET_SCALE") {
    Serial.print("SCALE,");
    Serial.println(HX711_SCALE, 6);
    return;
  }

  if (upper == "SAVE_SCALE") {
    saveScaleToEEPROM(HX711_SCALE);
    return;
  }

  if (upper == "LOAD_SCALE") {
    float s;
    if (loadScaleFromEEPROM(s)) {
      doSetScale(s);
      Serial.println("# LOADED scale from EEPROM.");
    } else {
      Serial.println("# No valid scale in EEPROM.");
    }
    return;
  }

  Serial.println("# Unknown command.");
}

// ---------------- Setup / Loop ----------------
void setup() {
  Serial.begin(115200);

  scale.begin(HX711_DT_PIN, HX711_SCK_PIN);

  float storedScale = 0.0f;
  if (loadScaleFromEEPROM(storedScale)) {
    HX711_SCALE = storedScale;
    Serial.print("# Loaded scale from EEPROM: ");
    Serial.println(HX711_SCALE, 6);
  }

  scale.set_scale(HX711_SCALE);
  scale.tare();

  Serial.println("# TARTAN Nano (load cell only) ready.");
  Serial.print("# HX711_SCALE=");
  Serial.println(HX711_SCALE, 6);
}

void loop() {
  // ---- Read serial commands ----
  if (Serial.available()) {
    String cmd = Serial.readStringUntil('\n');
    handleCommand(cmd);
  }

  // ---- CSV stream ----
  // Only emit a row when the HX711 has a valid reading ready.
  // If not ready, the sample is skipped entirely — no 0.000 artefacts.
  if (streamEnabled) {
    unsigned long nowMs = millis();
    if (nowMs - lastStreamMs >= STREAM_INTERVAL_MS) {
      lastStreamMs = nowMs;
      if (scale.is_ready()) {
        float force_g = scale.get_units(1);
        Serial.print(nowMs);
        Serial.print(",");
        Serial.println(force_g, 3);
      }
    }
  }
}
