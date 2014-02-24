

enum OverSampleType {
    "none",
    "2x",
    "4x"
};

interface WaveShaperNode : AudioNode {

    attribute Float32Array? curve;
    attribute OverSampleType oversample;

};
