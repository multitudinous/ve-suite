#ifndef _BIV_TEXTURE_MANAGER_H_
#define _BIV_TEXTURE_MANAGER_H_
#ifdef WIN32 
#include <windows.h>
#endif
#ifdef  VE_PATENTED
//#include <gl/gl.h>
#include <iostream>
#include <vector>

class cfdTextureManager{
public:
   cfdTextureManager();
   cfdTextureManager(const cfdTextureManager& tm);
   virtual ~cfdTextureManager();

   enum DataType{SCALAR,VECTOR};
   enum PlayMode{PLAY,STOP};

   //add a vector field from a file
   void addFieldTextureFromFile(char* textureFile);

   void setPlayMode(PlayMode mode){_mode = mode;}
   //forwardBackward == -1 backward
   //forwardBackward == 1 forward
   void setDirection(int forwardBackward);

   //set the current frame
   void SetCurrentFrame(unsigned int whichFrame);

   float* getBoundingBox(){return _bbox;}
   int timeToUpdate(double curTime,double delay);

   //get the vector field at a given timestep
   unsigned char* dataField(int timeStep){return _dataFields.at(timeStep);}
   
   //get the next vector field
   unsigned char* getNextField(/*int plusNeg*/);
   unsigned int getNextFrame();
   //get the number of vector fields
   int numberOfFields(){return _dataFields.size();}

   //the resolution of the fields
   int* fieldResolution(){return _resolution;}

   //the current frame
   unsigned int GetCurrentFrame();

   //the data ranges
   float* dataRange(){return _range;}

   float* transientRange(){return _transientRange;}

   DataType GetDataType(int whichField){return _types.at(whichField);}

   //equal operator
   cfdTextureManager& operator=(const cfdTextureManager& tm); 
protected:
   int _curField;
   int* _resolution;
   std::vector<DataType> _types;
   float _bbox[6];
   float _range[2];
   float _transientRange[2];

   std::vector<unsigned char*> _dataFields;
   double _prevTime;
   int _direction;
   PlayMode _mode;
};
#endif //VE_PATENTED
#endif //_BIV_TEXTURE_MANAGER_H_
