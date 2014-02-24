

interface AudioProcessingEvent : Event {

    readonly attribute double playbackTime;
    readonly attribute AudioBuffer inputBuffer;
    readonly attribute AudioBuffer outputBuffer;

};
