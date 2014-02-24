

interface AudioBuffer {

    readonly attribute float sampleRate;
    readonly attribute long length;

    // in seconds 
    readonly attribute double duration;

    readonly attribute long numberOfChannels;

    Float32Array getChannelData(unsigned long channel);

};
