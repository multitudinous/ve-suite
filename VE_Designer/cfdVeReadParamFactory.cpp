#include "cfdVeReadParamFactory.h"
#include "cfdVeReadParam.h"

cfdVeReadParamFactory* cfdVeReadParamFactory::myinstance = 0;

cfdVeReadParamFactory& cfdVeReadParamFactory::getInstance()
{
  if (myinstance == 0)
  {
     myinstance = new cfdVeReadParamFactory();
  }
  return *myinstance;
}

cfdVeReadParam* 
cfdVeReadParamFactory::getParamFile(char* filename)
{
   std::string key(filename);

   if (mFileMap.find(key) == mFileMap.end())
   {
      mFileMap[key] = new cfdVeReadParam(filename);
   }
   return mFileMap[key];
}

cfdVeReadParamFactory::~cfdVeReadParamFactory()
{
   std::map<std::string, cfdVeReadParam*>::iterator itr;
   
   for (itr = mFileMap.begin(); itr != mFileMap.end(); ++itr)
   {
      delete itr->second;
   }
}
