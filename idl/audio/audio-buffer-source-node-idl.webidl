

interface AudioBufferSourceNode : AudioNode {

    attribute AudioBuffer? buffer;

    readonly attribute AudioParam playbackRate;

    attribute boolean loop;
    attribute double loopStart;
    attribute double loopEnd;

    void start(optional double when = 0, optional double offset = 0, optional double duration);
    void stop(optional double when = 0);

    attribute EventHandler onended;

};
