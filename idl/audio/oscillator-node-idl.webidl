

enum OscillatorType {
  "sine",
  "square",
  "sawtooth",
  "triangle",
  "custom"
};

interface OscillatorNode : AudioNode {

    attribute OscillatorType type;

    readonly attribute AudioParam frequency; // in Hertz
    readonly attribute AudioParam detune; // in Cents

    void start(double when);
    void stop(double when);
    void setPeriodicWave(PeriodicWave periodicWave);

    attribute EventHandler onended;

};
