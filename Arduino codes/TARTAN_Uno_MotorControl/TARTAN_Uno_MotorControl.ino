// ============================================================
// TARTAN – Motor Control Firmware
// Arduino Uno Rev 3
//
// Controls two synchronised NEMA 17 stepper motors via TMC2209
// stepper drivers. Provides homing (end-stop based),
// controlled movement at a defined speed and distance, and an
// emergency stop, all via a simple serial command interface.
//
// Commands can be sent directly from the Arduino IDE Serial Monitor
// (set to 115200 baud, Newline line ending) or from any
// serial terminal.
//
// ── Wiring ────────────────────────────────────────────────
//   D2  – X_STEP  (left  motor driver, step pulse)
//   D3  – X_DIR   (left  motor driver, direction)
//   D4  – Y_STEP  (right motor driver, step pulse)
//   D5  – Y_DIR   (right motor driver, direction)
//   D8  – Left  end-stop (normally open, active LOW)
//   D9  – Right end-stop (normally open, active LOW)
//
//   Both driver ENABLE pins should be wired to GND (always
//   enabled), or controlled separately if motor de-energisation
//   between moves is required.
//
// ── Motion calibration ────────────────────────────────────
//   STEPS_PER_MM must be set to match the lead screw pitch and
//   driver microstepping configuration of the build.
//   The value used in this study (400.0) corresponds to a
//   200 step/rev motor with 1/2 microstepping on a lead screw
//   with 2.5 mm pitch. Verify empirically before use:
//   command a known distance and measure the actual travel.
//
// ── Serial interface ──────────────────────────────────────
//   Baud: 115200  |  Line ending: Newline (\n)
//
//   Commands:
//     MOVE <mm> <UP|DOWN> <speed_cm_s>
//         Move the platform by <mm> millimetres in the specified
//         direction at <speed_cm_s> cm/s.
//         e.g.  MOVE 10 UP 2.0
//               MOVE 50 DOWN 1.0
//
//     HOME_HIT <speed_cm_s>
//         Drive the platform downward at the specified speed
//         until one or both end-stops are triggered, then stop.
//         e.g.  HOME_HIT 1.0
//
//     OFF
//         Stop all motor motion immediately.
//
//     HELP
//         Print a summary of available commands.
//
//   Event messages (responses from the board):
//     EVENT,MOVE_START,<mm>,<dir>  – move has begun
//     EVENT,MOVE_DONE              – move completed normally
//     EVENT,HOME_START             – homing has begun
//     EVENT,HOME_DONE              – end-stop reached
//     EVENT,HOME_TIMEOUT           – homing stopped after 30 s
//                                    safety timeout
//     EVENT,OFF                    – motors stopped by command
// ============================================================

#include <Arduino.h>

// ---------------- Pins ----------------
#define X_STEP_PIN    2
#define X_DIR_PIN     3
#define Y_STEP_PIN    4
#define Y_DIR_PIN     5
#define ENDSTOP_L_PIN 8
#define ENDSTOP_R_PIN 9

// ---------------- Config ----------------
const float STEPS_PER_MM           = 400.0f;
const bool  ENDSTOP_ACTIVE_LOW     = true;
const unsigned long HOME_TIMEOUT_MS = 30000;

// ---------------- Types ----------------
enum Mode { MODE_IDLE, MODE_MOVE, MODE_HOME };
enum Dir  { UP_DIR, DOWN_DIR };

// ---------------- State ----------------
Mode mode             = MODE_IDLE;
bool motorsEnabled    = false;
unsigned long lastStepMicros = 0;
unsigned int  stepDelayUs    = 1500;
long  stepsRemaining  = 0;
float activeMoveMm    = 0.0f;
Dir   activeMoveDir   = UP_DIR;
unsigned long homeStartMs = 0;

// ---------------- Helpers ----------------
bool endstopPressed(uint8_t pin) {
  return ENDSTOP_ACTIVE_LOW ? (digitalRead(pin) == LOW) : (digitalRead(pin) == HIGH);
}

bool homeReached() {
  return (endstopPressed(ENDSTOP_L_PIN) || endstopPressed(ENDSTOP_R_PIN));
}

void setDirection(Dir d) {
  digitalWrite(X_DIR_PIN, d == UP_DIR ? LOW : HIGH);
  digitalWrite(Y_DIR_PIN, d == UP_DIR ? LOW : HIGH);
}

unsigned int delayUsFromSpeedCmS(float v_cms) {
  if (v_cms < 0.05f) v_cms = 0.05f;
  float d = (248.0f / v_cms) - 16.0f;
  if (d < 50.0f)    d = 50.0f;
  if (d > 20000.0f) d = 20000.0f;
  return (unsigned int)(d + 0.5f);
}

void motorsOff() {
  motorsEnabled  = false;
  stepsRemaining = 0;
  mode           = MODE_IDLE;
  digitalWrite(X_STEP_PIN, LOW);
  digitalWrite(Y_STEP_PIN, LOW);
}

// ---------------- Command actions ----------------
void startMoveMm(float mm, Dir d, unsigned int speedUs) {
  if (mm < 0) mm = -mm;
  stepDelayUs    = speedUs;
  stepsRemaining = (long)(mm * STEPS_PER_MM + 0.5f);
  activeMoveMm   = mm;
  activeMoveDir  = d;
  setDirection(d);
  mode           = MODE_MOVE;
  motorsEnabled  = true;

  char buf[48];
  snprintf(buf, sizeof(buf), "EVENT,MOVE_START,%.2f,%s",
           mm, (d == UP_DIR) ? "UP" : "DOWN");
  Serial.println(buf);
}

void startHomeHit(unsigned int speedUs) {
  stepDelayUs   = speedUs;
  homeStartMs   = millis();
  setDirection(DOWN_DIR);
  mode          = MODE_HOME;
  motorsEnabled = true;
  Serial.println("EVENT,HOME_START");
}

// ---------------- Command parser ----------------
void handleCommand(String cmd) {
  cmd.trim();
  if (cmd.length() == 0) return;

  String upper = cmd;
  upper.toUpperCase();

  if (upper == "HELP") {
    Serial.println("# Commands: MOVE <mm> <UP|DOWN> <speed> | HOME_HIT <speed> | OFF");
    return;
  }

  if (upper == "OFF") {
    motorsOff();
    Serial.println("EVENT,OFF");
    return;
  }

  if (upper.startsWith("HOME_HIT")) {
    int spc = cmd.indexOf(' ');
    if (spc < 0) { Serial.println("# Usage: HOME_HIT <speed_cm_s>"); return; }
    float v_cms = cmd.substring(spc + 1).toFloat();
    startHomeHit(delayUsFromSpeedCmS(v_cms));
    return;
  }

  if (upper.startsWith("MOVE")) {
    cmd.replace(":", " ");
    cmd.trim();
    int p1 = cmd.indexOf(' ');
    int p2 = cmd.indexOf(' ', p1 + 1);
    int p3 = cmd.indexOf(' ', p2 + 1);
    if (p1 < 0 || p2 < 0 || p3 < 0) {
      Serial.println("# Usage: MOVE <mm> <UP|DOWN> <speed_cm_s>");
      return;
    }
    float  mm    = cmd.substring(p1 + 1, p2).toFloat();
    String dStr  = cmd.substring(p2 + 1, p3); dStr.trim(); dStr.toUpperCase();
    float  v_cms = cmd.substring(p3 + 1).toFloat();
    Dir    dir   = (dStr == "DOWN") ? DOWN_DIR : UP_DIR;
    startMoveMm(mm, dir, delayUsFromSpeedCmS(v_cms));
    return;
  }

  Serial.println("# Unknown command. Type HELP.");
}

// ---------------- Setup / Loop ----------------
void setup() {
  pinMode(X_STEP_PIN,    OUTPUT);
  pinMode(X_DIR_PIN,     OUTPUT);
  pinMode(Y_STEP_PIN,    OUTPUT);
  pinMode(Y_DIR_PIN,     OUTPUT);
  pinMode(ENDSTOP_L_PIN, INPUT_PULLUP);
  pinMode(ENDSTOP_R_PIN, INPUT_PULLUP);

  Serial.begin(115200);
  motorsOff();
  Serial.println("# TARTAN Uno (motors only) ready.");
}

void loop() {
  // ---- Serial commands ----
  // When idle: read full commands normally.
  // When moving: only check for an "OFF" abort — checked once per loop
  //              iteration so it never blocks the step-timing path.
  if (!motorsEnabled && Serial.available()) {
    String cmd = Serial.readStringUntil('\n');
    handleCommand(cmd);
  } else if (motorsEnabled && Serial.available()) {
    // Peek at the incoming byte without blocking.
    // We only act on 'O' (start of "OFF"); ignore everything else mid-move
    // to avoid disrupting step timing with a full readStringUntil.
    int c = Serial.peek();
    if (c == 'O' || c == 'o') {
      String cmd = Serial.readStringUntil('\n');
      cmd.trim();
      String u = cmd; u.toUpperCase();
      if (u == "OFF") {
        motorsOff();
        Serial.println("EVENT,OFF");
      }
    } else {
      Serial.read(); // discard — not an abort command
    }
  }

  // ---- Step generation — ALL the loop does during motion ----
  if (motorsEnabled && (mode == MODE_MOVE || mode == MODE_HOME)) {
    unsigned long nowUs = micros();
    if (nowUs - lastStepMicros >= stepDelayUs) {
      lastStepMicros = nowUs;
      digitalWrite(X_STEP_PIN, HIGH);
      digitalWrite(Y_STEP_PIN, HIGH);
      delayMicroseconds(2);
      digitalWrite(X_STEP_PIN, LOW);
      digitalWrite(Y_STEP_PIN, LOW);
      if (mode == MODE_MOVE && stepsRemaining > 0) stepsRemaining--;
    }
  }

  // ---- Motion stop logic ----
  if (motorsEnabled && mode == MODE_MOVE && stepsRemaining <= 0) {
    motorsOff();
    Serial.println("EVENT,MOVE_DONE");
  }

  if (motorsEnabled && mode == MODE_HOME) {
    if (homeReached()) {
      motorsOff();
      Serial.println("EVENT,HOME_DONE");
    } else if (millis() - homeStartMs > HOME_TIMEOUT_MS) {
      motorsOff();
      Serial.println("EVENT,HOME_TIMEOUT");
    }
  }
}
