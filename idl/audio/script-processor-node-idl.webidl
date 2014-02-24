

interface ScriptProcessorNode : AudioNode {

    attribute EventHandler onaudioprocess;

    readonly attribute long bufferSize;

};
