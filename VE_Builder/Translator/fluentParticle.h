#ifndef FLUENTPARTICLE_H
#define FLUENTPARTICLE_H

//#include <gmtl/Vec.h>
//#include <gmtl/Generate.h>
#include <iostream>
#include <fstream>
#include <string>
class fluentParticle
{
   public:
      
      int mID;
      float mTime;
      int mTimeSteps;
      float mPosX,mPosY,mPosZ;
      float mVelocityU,mVelocityV,mVelocityW;
      float mDiameter;
      float mDensity;
      float mTemp;
      float mMass;
      

};
#endif
