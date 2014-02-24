

interface AudioListener {

    attribute double dopplerFactor;
    attribute double speedOfSound;

    // Uses a 3D cartesian coordinate system 
    void setPosition(double x, double y, double z);
    void setOrientation(double x, double y, double z, double xUp, double yUp, double zUp);
    void setVelocity(double x, double y, double z);

};
