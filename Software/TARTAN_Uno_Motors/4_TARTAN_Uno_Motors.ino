
#include <Arduino.h>

// ============================================================
// TARTAN — Arduino Uno
// Motors + HOME and TOP endstops
//
// ENDSTOPS — ACTIVE LOW
//
// HOME (D8):
//   Bottom travel limit
//   Stops ALL downward movement
//   Used for homing
//
// TOP (D9):
//   Upper travel limit
//   Stops ALL upward movement
//
// Moving away from a pressed endstop is always allowed.
//
// COMMANDS:
//   MOVE <mm> <UP|DOWN> <speed_cm_s>
//   HOME_HIT <speed_cm_s>
//   OFF
//   HELP
//
// EVENTS:
//   EVENT,MOVE_START,<mm>,<dir>
//   EVENT,MOVE_DONE
//   EVENT,HOME_START
//   EVENT,HOME_DONE
//   EVENT,HOME_TIMEOUT
//   EVENT,ESTOP
//   EVENT,BOTTOM_LIMIT
//   STEP_COUNT,<issued pulses since MOVE began>
//   EVENT,OFF
// ============================================================


// ---------------- Pins ----------------

// Motor X
#define X_STEP_PIN        2
#define X_DIR_PIN         3

// Motor Y
#define Y_STEP_PIN        4
#define Y_DIR_PIN         5

// Endstops
#define ENDSTOP_HOME_PIN  8
#define ENDSTOP_TOP_PIN   9


// ---------------- Configuration ----------------

const float STEPS_PER_MM = 400.0f;

const bool ENDSTOP_ACTIVE_LOW = true;

const unsigned long HOME_TIMEOUT_MS = 40000;


// ---------------- Types ----------------

enum Mode {
  MODE_IDLE,
  MODE_MOVE,
  MODE_HOME
};

enum Dir {
  UP_DIR,
  DOWN_DIR
};


// ---------------- State ----------------

Mode mode = MODE_IDLE;

bool motorsEnabled = false;

unsigned long lastStepMicros = 0;
unsigned int stepDelayUs = 1500;

long stepsRemaining = 0;
unsigned long issuedMoveSteps = 0;
unsigned long lastStepReportMs = 0;
const unsigned long STEP_REPORT_INTERVAL_MS = 100UL;

float activeMoveMm = 0.0f;
Dir activeMoveDir = UP_DIR;

unsigned long homeStartMs = 0;

// Non-blocking serial command buffer
char serialBuffer[64];
uint8_t serialIndex = 0;


// ============================================================
// ENDSTOP FUNCTIONS
// ============================================================

bool endstopPressed(uint8_t pin) {

  return ENDSTOP_ACTIVE_LOW
         ? digitalRead(pin) == LOW
         : digitalRead(pin) == HIGH;
}


// Bottom HOME endstop
bool homeReached() {

  return endstopPressed(ENDSTOP_HOME_PIN);
}


// Top travel limit
bool topLimitPressed() {

  return endstopPressed(ENDSTOP_TOP_PIN);
}


// ============================================================
// MOTOR FUNCTIONS
// ============================================================

void setDirection(Dir d) {

  digitalWrite(
    X_DIR_PIN,
    d == UP_DIR ? LOW : HIGH
  );

  digitalWrite(
    Y_DIR_PIN,
    d == UP_DIR ? LOW : HIGH
  );
}


// Convert speed in cm/s to motor step delay.
unsigned int delayUsFromSpeedCmS(float v_cms) {

  if (v_cms < 0.05f) {
    v_cms = 0.05f;
  }

  float d = (248.0f / v_cms) - 16.0f;

  if (d < 50.0f) {
    d = 50.0f;
  }

  if (d > 20000.0f) {
    d = 20000.0f;
  }

  return (unsigned int)(d + 0.5f);
}


// Stop all motor stepping.
void motorsOff() {

  motorsEnabled = false;
  stepsRemaining = 0;
  mode = MODE_IDLE;

  digitalWrite(X_STEP_PIN, LOW);
  digitalWrite(Y_STEP_PIN, LOW);
}


// Report the actual STEP pulses issued by the Uno, not measured shaft rotation.
// Report no more than once per 100 ms to minimise serial timing interference.
void reportMoveSteps() {
  Serial.print(F("STEP_COUNT,"));
  Serial.println(issuedMoveSteps);
  lastStepReportMs = millis();
}

// ============================================================
// ENDSTOP SAFETY
// ============================================================

// Check whether the active movement has reached a limit.
// Returns true if the motors were stopped.
bool checkEndstopLimits() {

  if (!motorsEnabled) {
    return false;
  }

  // ---------------- TOP LIMIT ----------------
  // Prevent UPWARD movement beyond the top endstop.

  if (
    mode == MODE_MOVE &&
    activeMoveDir == UP_DIR &&
    topLimitPressed()
  ) {

    reportMoveSteps();
    motorsOff();

    Serial.println("EVENT,ESTOP");

    return true;
  }


  // ---------------- BOTTOM LIMIT ----------------
  // Prevent DOWNWARD movement beyond the HOME endstop.

  if (
    mode == MODE_MOVE &&
    activeMoveDir == DOWN_DIR &&
    homeReached()
  ) {

    reportMoveSteps();
    motorsOff();

    Serial.println("EVENT,BOTTOM_LIMIT");

    return true;
  }


  // ---------------- HOMING ----------------
  // Stop homing when the bottom endstop is pressed.

  if (
    mode == MODE_HOME &&
    homeReached()
  ) {

    motorsOff();

    Serial.println("EVENT,HOME_DONE");

    return true;
  }

  return false;
}


// ============================================================
// MOVEMENT COMMANDS
// ============================================================

void startMoveMm(
  float mm,
  Dir d,
  unsigned int speedUs
) {

  if (mm < 0) {
    mm = -mm;
  }

  // Do not start movement towards an already pressed limit.
  if (d == DOWN_DIR && homeReached()) {

    motorsOff();

    Serial.println("EVENT,BOTTOM_LIMIT");

    return;
  }

  if (d == UP_DIR && topLimitPressed()) {

    motorsOff();

    Serial.println("EVENT,ESTOP");

    return;
  }

  stepDelayUs = speedUs;

  stepsRemaining =
    (long)(mm * STEPS_PER_MM + 0.5f);

  activeMoveMm = mm;
  activeMoveDir = d;
  issuedMoveSteps = 0;
  lastStepReportMs = millis();

  setDirection(d);

  // Start the timing from the beginning of this movement.
  lastStepMicros = micros();

  mode = MODE_MOVE;
  motorsEnabled = true;

  // Send movement start event.
  Serial.print("EVENT,MOVE_START,");
  Serial.print(mm, 2);
  Serial.print(",");
  Serial.println(d == UP_DIR ? "UP" : "DOWN");
  reportMoveSteps();  // Establish a real zero before any pulses.
}


// ============================================================
// HOMING COMMAND
// ============================================================

void startHomeHit(unsigned int speedUs) {

  // If already at HOME, there is no need to move.
  if (homeReached()) {

    motorsOff();

    Serial.println("EVENT,HOME_START");
    Serial.println("EVENT,HOME_DONE");

    return;
  }

  stepDelayUs = speedUs;

  homeStartMs = millis();

  // Homing always moves DOWN.
  setDirection(DOWN_DIR);

  activeMoveDir = DOWN_DIR;

  lastStepMicros = micros();

  mode = MODE_HOME;
  motorsEnabled = true;

  Serial.println("EVENT,HOME_START");
}


// ============================================================
// COMMAND PARSER
// ============================================================

void handleCommand(String cmd) {

  cmd.trim();

  if (cmd.length() == 0) {
    return;
  }

  String upper = cmd;
  upper.toUpperCase();


  // ---------------- HELP ----------------

  if (upper == "HELP") {

    Serial.println(
      "# Commands: MOVE <mm> <UP|DOWN> <speed> | "
      "HOME_HIT <speed> | OFF"
    );

    return;
  }


  // ---------------- OFF ----------------

  if (upper == "OFF") {

    if (mode == MODE_MOVE) reportMoveSteps();
    motorsOff();

    Serial.println("EVENT,OFF");

    return;
  }


  // Ignore movement commands if motors are already moving.
  if (motorsEnabled) {

    Serial.println("# Busy. Use OFF to stop.");

    return;
  }


  // ---------------- HOME ----------------

  if (upper.startsWith("HOME_HIT")) {

    int spc = cmd.indexOf(' ');

    if (spc < 0) {

      Serial.println(
        "# Usage: HOME_HIT <speed_cm_s>"
      );

      return;
    }

    float v_cms =
      cmd.substring(spc + 1).toFloat();

    startHomeHit(
      delayUsFromSpeedCmS(v_cms)
    );

    return;
  }


  // ---------------- MOVE ----------------

  if (upper.startsWith("MOVE")) {

    cmd.replace(":", " ");
    cmd.trim();

    int p1 = cmd.indexOf(' ');
    int p2 = cmd.indexOf(' ', p1 + 1);
    int p3 = cmd.indexOf(' ', p2 + 1);

    if (p1 < 0 || p2 < 0 || p3 < 0) {

      Serial.println(
        "# Usage: MOVE <mm> <UP|DOWN> <speed_cm_s>"
      );

      return;
    }

    float mm =
      cmd.substring(p1 + 1, p2).toFloat();

    String dStr =
      cmd.substring(p2 + 1, p3);

    dStr.trim();
    dStr.toUpperCase();

    float v_cms =
      cmd.substring(p3 + 1).toFloat();

    Dir dir =
      dStr == "DOWN" ? DOWN_DIR : UP_DIR;

    startMoveMm(
      mm,
      dir,
      delayUsFromSpeedCmS(v_cms)
    );

    return;
  }


  Serial.println(
    "# Unknown command. Type HELP."
  );
}


// ============================================================
// NON-BLOCKING SERIAL READER
// ============================================================

void readSerialCommands() {

  while (Serial.available()) {

    char c = (char)Serial.read();

    if (c == '\n' || c == '\r') {

      if (serialIndex > 0) {

        serialBuffer[serialIndex] = '\0';

        String cmd = String(serialBuffer);
        serialIndex = 0;

        // During movement, only allow OFF.
        if (motorsEnabled) {

          cmd.trim();
          cmd.toUpperCase();

          if (cmd == "OFF") {

            if (mode == MODE_MOVE) reportMoveSteps();
            motorsOff();

            Serial.println("EVENT,OFF");
          }
        }

        else {

          handleCommand(cmd);
        }
      }
    }

    else if (serialIndex < sizeof(serialBuffer) - 1) {

      serialBuffer[serialIndex++] = c;
    }

    else {

      // Discard an excessively long command.
      serialIndex = 0;
    }
  }
}


// ============================================================
// SETUP
// ============================================================

void setup() {

  // Motor outputs
  pinMode(X_STEP_PIN, OUTPUT);
  pinMode(X_DIR_PIN, OUTPUT);

  pinMode(Y_STEP_PIN, OUTPUT);
  pinMode(Y_DIR_PIN, OUTPUT);

  // Endstops — active LOW
  pinMode(ENDSTOP_HOME_PIN, INPUT_PULLUP);
  pinMode(ENDSTOP_TOP_PIN, INPUT_PULLUP);

  Serial.begin(115200);

  motorsOff();

  Serial.println(
    "# TARTAN Uno (motors + endstops) ready."
  );
}


// ============================================================
// MAIN LOOP
// ============================================================

void loop() {

  // ----------------------------------------------------------
  // 1. CHECK SERIAL COMMANDS
  // ----------------------------------------------------------

  readSerialCommands();


  // ----------------------------------------------------------
  // 2. CHECK ENDSTOPS BEFORE STEPPING
  // ----------------------------------------------------------

  // Stops movement towards whichever endstop is pressed.
  checkEndstopLimits();


  // ----------------------------------------------------------
  // 3. CHECK HOMING TIMEOUT
  // ----------------------------------------------------------

  if (
    motorsEnabled &&
    mode == MODE_HOME &&
    millis() - homeStartMs >= HOME_TIMEOUT_MS
  ) {

    motorsOff();

    Serial.println("EVENT,HOME_TIMEOUT");
  }


  // ----------------------------------------------------------
  // 4. GENERATE MOTOR STEPS
  // ----------------------------------------------------------

  if (
    motorsEnabled &&
    (mode == MODE_MOVE || mode == MODE_HOME)
  ) {

    unsigned long nowUs = micros();

    if (
      nowUs - lastStepMicros >= stepDelayUs
    ) {

      // Final limit check immediately before stepping.
      if (checkEndstopLimits()) {
        return;
      }

      lastStepMicros = nowUs;

      // Step both motors together.
      digitalWrite(X_STEP_PIN, HIGH);
      digitalWrite(Y_STEP_PIN, HIGH);

      delayMicroseconds(2);

      digitalWrite(X_STEP_PIN, LOW);
      digitalWrite(Y_STEP_PIN, LOW);

      // Count steps only for normal movements.
      if (
        mode == MODE_MOVE &&
        stepsRemaining > 0
      ) {

        stepsRemaining--;
        issuedMoveSteps++;
      }
    }
  }


  // Periodic Uno pulse-count telemetry. Never infer pulses from time.
  if (motorsEnabled && mode == MODE_MOVE &&
      millis() - lastStepReportMs >= STEP_REPORT_INTERVAL_MS) {
    reportMoveSteps();
  }

  // ----------------------------------------------------------
  // 5. NORMAL MOVEMENT COMPLETION
  // ----------------------------------------------------------

  if (
    motorsEnabled &&
    mode == MODE_MOVE &&
    stepsRemaining <= 0
  ) {

    reportMoveSteps();
    motorsOff();

    Serial.println("EVENT,MOVE_DONE");
  }

}
