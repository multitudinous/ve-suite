#include <VjDesignObs_i.h>
#include <iostream>
#include <time.h>


VjDesignObs_i::VjDesignObs_i(char* filename)
{
   int temp =0;
   orb=CORBA::ORB_init(temp,0,"omniORB4");
   this->param_file =cfdVeReadParamFactory::getInstance().getParamFile(filename);
   //this->popsize = this->param_file->popsize;
   this->VeApp_local_params = new double[this->param_file->getPopSize()];
   this->VeApp_activeindex = new short[this->param_file->getPopSize()];

}
VjDesignObs_i::VjDesignObs_i()
{

}

/*VjDesignObs_i::~VjDesignObs_i()
{

}*/

void VjDesignObs_i::update()
{

}

void VjDesignObs_i::setClientInfoFlag(const short value)
{
   mValueLock.acquire();
   {
      this->b_getclientInfo = value;
   }
   mValueLock.release();
}

short VjDesignObs_i::getClientInfoFlag()
{
   mValueLock.acquire();
   {
     return this->b_getclientInfo;
   }
   mValueLock.release();

}

void VjDesignObs_i::setInteractiveDisplay_UI(const short value)
{
   mValueLock.acquire();
   {
      this->b_interactivedisplay_UI = value;
   }
   mValueLock.release();

}

short VjDesignObs_i::getInteractiveDisplay_UI()
{
   mValueLock.acquire();
   {
      return this->b_interactivedisplay_UI;
   }
   mValueLock.release();

}

void VjDesignObs_i::setInteractiveState_UI(const short value)
{

}

short VjDesignObs_i::getInteractiveState_UI()
{
   return this->b_interactivestate_UI;

}

void VjDesignObs_i::setInteractiveState_cfd(const short value)
{

}

short VjDesignObs_i::getInteractiveState_cfd()
{

   return this->b_interactivestate_cfd;
}
void VjDesignObs_i::setInteractiveGA(const short value)
{
   mValueLock.acquire();
   {
      this->b_interactiveGA = value;
   }
   mValueLock.release();

}

short VjDesignObs_i::getInteractiveGA()
{
   mValueLock.acquire();
   {
      return this->b_interactiveGA;
   }
   mValueLock.release();

}

void VjDesignObs_i::setActiveDesignParams(const VjDesignObs::active_flag &value)
{
   mValueLock.acquire();
   {
     for(int i=0; i<this->param_file->getNumDesignParams();i++)
     {
         this->VeApp_activeindex[i] = value[i];
     }

   }
   mValueLock.release();

}

short* VjDesignObs_i::getActiveDesignParams()
//VjDesignObs::active_flag* VjDesignObs_i::getActiveDesignParams()
{
   mValueLock.acquire();
   {
      return this->VeApp_activeindex;

   }
   mValueLock.release();
}

void VjDesignObs_i::setDesignParams(const VjDesignObs::design_params &value)
{
   mValueLock.acquire();
   {
       for(int i=0;i<this->param_file->getNumDesignParams();i++)
       {
            this->VeApp_local_params[i]= value[i];
       }
       this->VeApp_readytosendparams = true;

   }
   mValueLock.release();
                                                                    
}

double* VjDesignObs_i::getDesignParams()
//VjDesignObs::design_params* VjDesignObs_i::getDesignParams()
{
   mValueLock.acquire();
   {
      return this->VeApp_local_params;
      this->VeApp_readytosendparams = false;
   }
   mValueLock.release();

}

void VjDesignObs_i :: setVRserverName(const char* name)
{
   
}

char* VjDesignObs_i::getVRserverName()
//VjDesignObs::file_name* VjDesignObs_i ::getVRserver_name()
{

   return this->b_VRservername;
}

void VjDesignObs_i :: setMode_oldModel(const short value)
{

}

short VjDesignObs_i ::getMode_oldModel()
{
   return this->b_mode;
}

void VjDesignObs_i::setTrans_oldModel(const VjDesignObs::design_params &value)
{

}

double* VjDesignObs_i::getTrans_oldModel()
//VjDesignObs::design_params* VjDesignObs_i::getTrans_oldModel()
{
   return this->b_trans;
}

void VjDesignObs_i::setRot_oldModel(const VjDesignObs::design_params &value)
{
   
}

double* VjDesignObs_i::getRot_oldModel()
//VjDesignObs::design_params* VjDesignObs_i::getRot_oldModel()
{
   return this->b_rot;

}

void VjDesignObs_i::setNewVtkFileName(const char* name)
{

}

char* VjDesignObs_i::getNewVtkFileName()
//VjDesignObs::file_name* VjDesignObs_i::getNewVtkFileName()
{
   return this->b_vtkfilename;
}

void VjDesignObs_i::setCheckedGene(const short value)
{

}

short VjDesignObs_i::getCheckedGene()
{
   return this->b_checkedgene;

}

void VjDesignObs_i::setMutationRate(const double value)
{

}
double VjDesignObs_i::getMutationRate()
{
   return this->b_mutationrate;
}

void VjDesignObs_i::setCrossoverRate(const double value)
{

}


double VjDesignObs_i::getCrossoverRate()
{
   return this->b_crossoverrate;
}


void VjDesignObs_i::setSaveMode(const short value)
{
   

}

short VjDesignObs_i::getSaveMode()
{
   return this->b_savemode;

}

void VjDesignObs_i::setDisplayMode(const short value)
{
   
}

short VjDesignObs_i::getDisplayMode()
{
   return this->b_displaymode;
}
