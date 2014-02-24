

enum ChannelCountMode {
    "max",
    "clamped-max",
    "explicit"
};

enum ChannelInterpretation {
    "speakers",
    "discrete"
};

interface AudioNode : EventTarget {

    void connect(AudioNode destination, optional unsigned long output = 0, optional unsigned long input = 0);
    void connect(AudioParam destination, optional unsigned long output = 0);
    void disconnect(optional unsigned long output = 0);

    readonly attribute AudioContext context;
    readonly attribute unsigned long numberOfInputs;
    readonly attribute unsigned long numberOfOutputs;

    // Channel up-mixing and down-mixing rules for all inputs.
    attribute unsigned long channelCount;
    attribute ChannelCountMode channelCountMode;
    attribute ChannelInterpretation channelInterpretation;

};
