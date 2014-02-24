

enum PanningModelType {
  "equalpower",
  "HRTF"
};

enum DistanceModelType {
  "linear",
  "inverse",
  "exponential"
};

interface PannerNode : AudioNode {

    // Default for stereo is HRTF 
    attribute PanningModelType panningModel;

    // Uses a 3D cartesian coordinate system 
    void setPosition(double x, double y, double z);
    void setOrientation(double x, double y, double z);
    void setVelocity(double x, double y, double z);

    // Distance model and attributes 
    attribute DistanceModelType distanceModel;
    attribute double refDistance;
    attribute double maxDistance;
    attribute double rolloffFactor;

    // Directional sound cone 
    attribute double coneInnerAngle;
    attribute double coneOuterAngle;
    attribute double coneOuterGain;

};
