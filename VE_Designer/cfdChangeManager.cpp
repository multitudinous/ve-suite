#include "cfdChangeManager.h"
#include "cfdVeReadParam.h"

cfdChangeManager* cfdChangeManager::myinstance = 0;

cfdChangeManager& cfdChangeManager::getInstance()
{
  if (myinstance == 0)
  {
     myinstance = new cfdChangeManager();
  }
  return *myinstance;
}

cfdVeReadParm* cfdChangeManager::getParamFile(char* filename)
{
   std::string key(filename);

   if (mFileMap.find(key) == mFileMap.end())
   {
      mFileMap[key] = new cfdVeReadParam(filename);
   }
   return mFileMap[key];
}

void cfdChangeManager::setInfoFromParamFile(char* filename)
{
  cfdVeReadParam* paramfile ;
  paramfile =(this->changemanager).getParamFile(filename);

  this->vrservername = paramfile->getVRserverName();
  this->cfdservername = paramfile->getCFDserverName();
  this->cfdpackage = paramfile->getCFDpackageName();
  
  this->keepoldmode = paramfile->getModeForOldModel();
  for(int i=0;i<3;i++)
  {
      this->oldmodetrans[i]= (paramfile->getTransForOldModel)[i];
      this->oldmoderot[i] = (paramfile->getRotForOldModel)[i];
  }

  this->newvtkfilename = paramfile->getNewVtkFileName();
  this->savemode = paramfile->getDesignSaveMode();
  this->designdisplaymode = paramfile->getDesignDisplayMode();

  this->checkedgene = paramfile->getCheckedGene();
  this->directory = paramfile->getDirectoryName();
  

  this->popsize = paramfile->getPopSize();
  this->totalruns = paramfile->getTotalRuns();
  this->initialtype = paramfile->getInitialType();
  this->totalmatingevents = paramfile->getTotalMatingEvents();
  this->initialpopfilename = paramfile->getInitialPopFileName();
  this->boundaryfilename = paramfile->getBoundaryFileName();
  
  delete paramfile;
  
  
   
}
cfdChangeManager::~cfdChangeManager()
{
   std::map<std::string, cfdVeReadParam*>::iterator itr;
   
   for (itr = mFileMap.begin(); itr != mFileMap.end(); ++itr)
   {
      delete itr->second;
   }
}

void cfdChangeManager::setOPID(int value)
{
   this->opID = value;
}

int cfdChangeManager::getOPID()
{
   if((this->opID>MaxOPID)||(this->opID<MinOPID))
   {  
      this->opID = -1;
   }

   return this->opID;
}
void cfdChangeManager::setVRserverName(char* name)
{
   this->vrservername = name;
}

char* cfdChangeManager::getVRserverName()
{
   return this->vrservername;
}
void cfdChangeManager::setModeForOldModel(bool mode)
{
   this->keepoldmodel = mode;
}

bool cfdChangeManager::getModeForOldModel()
{
   return this->keepoldmodel;
}

void cfdChangeManager::setTransForOldModel(float* trans)
{
   
   for(int i=0; i<3;i++)
   {
      this->oldmodeltrans[i]= trans[i];
   }
}

float* cfdChangeManager::getTransForOldModel()
{
   return this->oldmodeltrans;
}

void cfdChangeManager::setRotForOldModel(float* rot)
{
   for(int i=0; i<3; i++)
   {
      this->oldmodelrot[i]= rot[i];
   }
}

float* cfdChangeManager::getRotForOldModel()
{
   return this->oldmodelrot;
}

void cfdChangeManager::setNewVtkFileName(char* name)
{
   this->newvtkfilename = name;
}

char* cfdChangeManager:: getNewVtkFileName()
{
   return this->newvtkfilename;
}

void cfdChangeManager::setCurrentDesignParams(float* value)
{
   for(int i=0; i<this->numdesignparams;i++)
   {
      this->currentdesignparams[i] = value[i];
   }
}

float* cfdChangeManager::getCurrentDesignParams()
{
   return this->currentdesignparams;
}

void cfdChangeManager::setActiveParams(int* value)
{
   for(int i=0; i<this->numdesignparams;i++)
   {
      this->activeparamsflag[i] = value[i];
   }
}

int* cfdChangeManager::getActiveParams()
{
   return this->activeparamsflag;
}

void cfdChangeManager::setCheckedGene(int value)
{
   this->checkedgene = value;
}

int cfdChangeManager::getCheckedGene()
{
   return this->checkedgene;
}

void cfdChangeManager::setCrossOverRate(float value)
{
   this->crossoverrate = value;
}

float cfdChangeManager::getCrossOverRate()
{
   return this->crossoverrate;
}

void cfdChangeManager::setMutationRate(float value)
{
   this->mutationrate = value;
}

float cfdChangeManager::getMutationRate()
{
   return this->mutationrate;
}

int cfdChangeManager::getInitialType()
{
   return this->initialtype;
}

int cfdChangeManager::getNumDesignParams()
{
   return this->numdesignparams;
}

int cfdChangeManager::getPopSize()
{
   return this->popsize;
}   

int cfdChangeManager::getTotalRuns()
{
   return this->totalruns;
}

int cfdChangeManager::getTotalMatingEvents()
{
   return this->totalmatingevents;
}
char* cfdChangeManager::getBoundaryFileName()
{
   return this->boundaryfilename;
}

char* cfdChangeManager::getInitialPopFileName()
{
   return this->initialpopfilename;
}

char* cfdChangeManager::getDirectoryName()
{
   return this->directory;
}

bool cfdChangeManager::getInteractiveDisplayFlag()
{
   return this->interactivedisplayflag;
}

void cfdChangeManager::setChangeType(int value)
{
   this->changetype = value;
}

int cfdChangeManager::getChangeType()
{
   return this->changetype;

}

int cfdChangeManager::getPopSize()
{
   return this->popsize;
}

int cfdChangeManager::getNumDesignParams()
{
   return this->numdesignparams;
}
void cfdChangeManager::setSaveMode(bool value)
{
   this->savemode = value;
}

bool cfdChangeManager::getSaveModel()
{
   return this->savemode;
}

void cfdChangeManager::setDesignDisplayMode(bool value)
{
   this->designdisplaymode = value;
}

bool cfdChangeManager::getDesignDisplyMode()
{
   return this->designdisplaymode;
}

void cfdChangeManager::setGADisplayMode(bool value)
{
   this->GAdispalymode = value;
   
}

bool cfdChangeManager::getGADisplayMode()
{
   return this->GAdisplaymode;
}
