#include "cfdVeReadParam.h"

cfdVeReadParam::cfdVeReadParam(char * filename)
{
   this->popsize =1;
   this->totalruns = 1;
   this->totalmatingevents = 0;
   this->oldmodeltrans = new float[3];
   this->oldmodelrot = new float[3];
   this->dataRead(filename);
   this->currentdesignparams  = new float[this->numdesignparams];
}

void cfdVeReadParam::dataRead(char *filename)
{
   std::ifstream inFile(filename, std::ios::in);
   if (!inFile)
   {
      std::cout << "[ERR] Could not open param file \"" << filename << "\""
                << std::endl;
   }
   this->activeGA = this->gaParamRead (inFile);
   std::cout<<"|  Finished reading the GAparameter file." <<std::endl;
   this->designParamRead(inFile);
   std::cout<<"|  Finished reading the Designparameter file." <<std::endl;
}

bool cfdVeReadParam::gaParamRead(std::ifstream &inFile)
{
   int numgenes;
   inFile >>numgenes;
   inFile.getline(textLine, 256);

   if(numgenes ==1)
   {
      this->popsize = numgenes;
      return 0;
   }
   else if (numgenes<=4 &&numgenes>1)
   {
      this->activeGA = 0;
      this->popsize = numgenes;
      return 0;
   }
   else 
   {
      this->activeGA = 1;
      this->popsize = numgenes;
      inFile >> this->totalruns;
      inFile.getline (textLine,256);
      inFile >> this->totalmatingevents;
      inFile.getline (textLine,256);
      return 1;
      
   }
   
}

void cfdVeReadParam::designParamRead(std::ifstream &inFile)
{
   inFile>>this->initialtype;
   inFile.getline(textLine,256);
   inFile>>this->numdesignparams;
   inFile.getline(textLine,256);
   switch(this->initialtype)
   {
      case 0: //read initial design candidates from the file
         inFile>>this->initialpop_filename;
         inFile.getline(textLine,256);
         inFile>>this->maxmin_filename;
         inFile.getline(textLine,256);
         break;
      case 1: //computer generates the initial design candidates
         inFile>>maxmin_filename;
         inFile.getline(textLine,256);
         break;
      default:
         std::cout<<" Invalid initial type, check the param file. "<<std::endl;
         exit(1);

   }
   
   std::cout<<" The population size is "<<this->popsize << std::endl;
   std::cout<<" The design candidates are type "<< this->initialtype<<std::endl;
   std::cout<<" The number of design parameters is "<<this->numdesignparams<<std::endl;
   
}

void cfdVeReadParam::setVRserverName(char* name)
{
   this->vrservername = name;
}

char* cfdVeReadParam::getVRserverName()
{
   return this->vrservername;
}
void cfdVeReadParam::setModeForOldModel(bool mode)
{
   this->keepoldmodel = mode;
}

bool cfdVeReadParam::getModeForOldModel()
{
   return this->keepoldmodel;
}

void cfdVeReadParam::setTransForOldModel(float* trans)
{
   
   for(int i=0; i<3;i++)
   {
      this->oldmodeltrans[i]= trans[i];
   }
}

float* cfdVeReadParam::getTransForOldModel()
{
   return this->oldmodeltrans;
}

void cfdVeReadParam::setRotForOldModel(float* rot)
{
   for(int i=0; i<3; i++)
   {
      this->oldmodelrot[i]= rot[i];
   }
}

float* cfdVeReadParam::getRotForOldModel()
{
   return this->oldmodelrot;
}

void cfdVeReadParam::setNewVtkFileName(char* name)
{
   this->newvtkfilename = name;
}

char* cfdVeReadParam:: getNewVtkFileName()
{
   return this->newvtkfilename;
}

void cfdVeReadParam::setCurrentDesignParams(float* value)
{
   for(int i=0; i<this->numdesignparams;i++)
   {
      this->currentdesignparams[i] = value[i];
   }
}

float* cfdVeReadParam::getCurrentDesignParams()
{
   return this->currentdesignparams;
}

void cfdVeReadParam::setActiveParams(int* value)
{
   for(int i=0; i<this->numdesignparams;i++)
   {
      this->activeparamsflag[i] = value[i];
   }
}

int* cfdVeReadParam::getActiveParams()
{
   return this->activeparamsflag;
}

void cfdVeReadParam::setCheckedGene(int value)
{
   this->checkedgene = value;
}

int cfdVeReadParam::getCheckedGene()
{
   return this->checkedgene;
}

void cfdVeReadParam::setCrossOverRate(float value)
{
   this->crossoverrate = value;
}

float cfdVeReadParam::getCrossOverRate()
{
   return this->crossoverrate;
}

void cfdVeReadParam::setMutationRate(float value)
{
   this->mutationrate = value;
}

float cfdVeReadParam::getMutationRate()
{
   return this->mutationrate;
}

int cfdVeReadParam::getInitialType()
{
   return this->initialtype;
}

int cfdVeReadParam::getNumDesignParams()
{
   return this->numdesignparams;
}

int cfdVeReadParam::getPopSize()
{
   return this->popsize;
}   

int cfdVeReadParam::getTotalRuns()
{
   return this->totalruns;
}

int cfdVeReadParam::getTotalMatingEvents()
{
   return this->totalmatingevents;
}
char* cfdVeReadParam::getBoundaryFileName()
{
   return this->maxmin_filename;
}

char* cfdVeReadParam::getInitialPopFileName()
{
   return this->initialpop_filename;
}

char* cfdVeReadParam::getDirectoryName()
{
   return this->directory;
}

bool cfdVeReadParam::getInteractiveDisplayFlag()
{
   return this->interactivedisplayflag;
}
