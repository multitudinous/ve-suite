//
// Copyright (c) 2009 Skew Matrix  Software LLC.
// All rights reserved.
//

#include <osgBullet/Export.h>
#include <osgBullet/GeometryOperation.h>
#include <osg/Geode>
#include <osg/Geometry>

namespace osgBullet {

class OSGBULLET_EXPORT DecimatorOp : public GeometryOperation
{
public:

    DecimatorOp(double sampleRatio=1.0, double maximumError=FLT_MAX);

    DecimatorOp( const DecimatorOp& rhs, const osg::CopyOp& copyOp=osg::CopyOp::SHALLOW_COPY );
    META_Object(osgBullet,DecimatorOp)


    void setSampleRatio(float sampleRatio) { _sampleRatio = sampleRatio; }
    float getSampleRatio() const { return _sampleRatio; }

    void setMaximumError(float error) { _maximumError = error; }
    float getMaximumError() const { return _maximumError; }

    void setDoTriStrip(bool on) { _triStrip = on; }
    bool getDoTriStrip() const { return _triStrip; }

    void setSmoothing(bool on) { _smoothing = on; }
    bool getSmoothing() const { return _smoothing; }

    void setIgnoreBoundries(bool setting){_ignoreBoundries = setting;}
    bool getIgnoreBoundries(){return _ignoreBoundries;}

    class ContinueDecimationCallback : public osg::Referenced
        {
            public:
                /** return true if mesh should be continued to be decimated, return false to stop Decimation.*/
                virtual bool continueDecimation(const DecimatorOp& decimator, float nextError, unsigned int numOriginalPrimitives, unsigned int numRemainingPrimitives) const
                {
                    return decimator.continueDecimationImplementation(nextError, numOriginalPrimitives, numRemainingPrimitives);
                }
            
            protected:
                virtual ~ContinueDecimationCallback() {}
        };
     void setContinueDecimationCallback(ContinueDecimationCallback* cb) { _continueDecimationCallback = cb; }
     ContinueDecimationCallback* getContinueDecimationCallback() { return _continueDecimationCallback.get(); }
     const ContinueDecimationCallback* getContinueDecimationCallback() const { return _continueDecimationCallback.get(); }
        
        
     bool continueDecimation(float nextError, unsigned int numOriginalPrimitives, unsigned int numRemainingPrimitives) const
     {
         if (_continueDecimationCallback.valid()) return _continueDecimationCallback->continueDecimation(*this, nextError, numOriginalPrimitives, numRemainingPrimitives);
         else return continueDecimationImplementation(nextError, numOriginalPrimitives, numRemainingPrimitives);
     }
     virtual bool continueDecimationImplementation(float nextError, unsigned int numOriginalPrimitives, unsigned int numRemainingPrimitives) const
     {
         return ((float)numRemainingPrimitives > ((float)numOriginalPrimitives) * getSampleRatio()) && nextError<=getMaximumError();
        
     }

    virtual osg::Geometry* operator()( osg::Geometry& geom )
    {
        decimate( geom );
        return( &geom );
    }
    
     void decimate(osg::Geometry& geometry);

protected:
    
   double _sampleRatio;
   double _maximumError;
   bool  _triStrip;
   bool  _smoothing;
   bool  _ignoreBoundries;
        
    osg::ref_ptr<ContinueDecimationCallback> _continueDecimationCallback;
};

}
