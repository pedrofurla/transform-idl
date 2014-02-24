

interface DynamicsCompressorNode : AudioNode {

    readonly attribute AudioParam threshold; // in Decibels
    readonly attribute AudioParam knee; // in Decibels
    readonly attribute AudioParam ratio; // unit-less
    readonly attribute AudioParam reduction; // in Decibels
    readonly attribute AudioParam attack; // in Seconds
    readonly attribute AudioParam release; // in Seconds

};
