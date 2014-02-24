

interface AnalyserNode : AudioNode {

    // Real-time frequency-domain data 
    void getFloatFrequencyData(Float32Array array);
    void getByteFrequencyData(Uint8Array array);

    // Real-time waveform data 
    void getByteTimeDomainData(Uint8Array array);

    attribute unsigned long fftSize;
    readonly attribute unsigned long frequencyBinCount;

    attribute double minDecibels;
    attribute double maxDecibels;

    attribute double smoothingTimeConstant;

};
