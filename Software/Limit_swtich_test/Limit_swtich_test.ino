#define ENDSTOP_L_PIN 12
#define ENDSTOP_R_PIN 13

void setup() {
  Serial.begin(9600);
  pinMode(ENDSTOP_L_PIN, INPUT_PULLUP);
  pinMode(ENDSTOP_R_PIN, INPUT_PULLUP);
}

void loop() {
  Serial.print("L=");
  Serial.print(digitalRead(ENDSTOP_L_PIN));
  Serial.print("  R=");
  Serial.println(digitalRead(ENDSTOP_R_PIN));
  delay(200);
}
