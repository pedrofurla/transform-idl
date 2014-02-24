

enum BiquadFilterType {
  "lowpass",
  "highpass",
  "bandpass",
  "lowshelf",
  "highshelf",
  "peaking",
  "notch",
  "allpass"
};

interface BiquadFilterNode : AudioNode {

    attribute BiquadFilterType type;
    readonly attribute AudioParam frequency; // in Hertz
    readonly attribute AudioParam detune; // in Cents
    readonly attribute AudioParam Q; // Quality factor
    readonly attribute AudioParam gain; // in Decibels

    void getFrequencyResponse(Float32Array frequencyHz,
                              Float32Array magResponse,
                              Float32Array phaseResponse);

};
