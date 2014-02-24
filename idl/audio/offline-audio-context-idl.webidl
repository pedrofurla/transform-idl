
[Constructor(unsigned long numberOfChannels, unsigned long length, float sampleRate)]
interface OfflineAudioContext : AudioContext {

    void startRendering();
    
    attribute EventHandler oncomplete;

};
