#include "cfdVeReadParam.h"
#include "assert.h"

cfdVeReadParam::initialDefaultInfo()
{
   /*
    * initial the necessary variables in case that the param file doesn't provide necessary variables for calculation.
    *
    * by default: only calculate one case, no GA needed
    * 
    */
   this->popsize =1;
   this->totalruns = 1;
   this->totalmatingevents = 0;
   this->numdesignparams = 100; //the maximum design params
   
   /*
    * interactiveGA-- users can randomly pick one gene and see the results
    *                 or change crossover rate and/or mutation rate
    * interactivedesign -- 
    *                 1) using this funciton in the initial design candidates
    *                    to choose the better designs.
    *                 2) fining the active design params 
    *
    */
   this->interactiveGA = false; 
   this->interactivedesign = false;
   this->interactivedisplay = true;

   /*
    * other default values
    *
    */

   this->vrservername = "lego";
   this->cfdservername = "lego";
   this->keepoldmodel = true;
   
   for (int i=1; i<3;i++)
   {
      this->oldmodeltrans[i] =2.0;
      this->oldmodelrot[i] =0.0;
   }
   
   this->savemode = false;
   this->designdisplaymode = true;
   
   this->newvtkfilename ="flowdata.vtk";
   this->cfdpackage = "fluent";
   this->initialpopfilename = "initialpop.dat";
   this->boundaryfilename = "boundary.dat";
   
   this->checkedgene = 1;
   this->directory ="DATA";
   this->crossoverrate = 0.5;
   this->mutationrate = 0.2;
}   

cfdVeReadParam::cfdVeReadParam(char* filename)
{
   this->initialDefaultInfo();
   this->loadDataFromXML(filename);
   this->currentdesignparams  = new float[this->numdesignparams];
}

void cfdVeReadParam::loadDataFromXML(char* filename)
{
  	cppdom::ContextPtr context(new cppdom::Context);
	cppdom::Document doc(context);

	doc.loadFile(filename);
   
   cppdom::NodePtr ParamFileNode=doc.getChild("ParamFile");

   assert(ParamFileNode.get() != NULL);

   //n1--the first level nodes
   cppdom::NodeList n1 = ParamFileNode->getChildren();
   assert(n1.size()<1);
   
   for(cppdom::NodeList::iterator cur_n1_elt = n1.begin(); cur_n1_elt != n1.end(); ++cur_n1_elt)
   {
      cppdom::NodePtr cur_n1_node = *cur_n1_elt;
      if("General"==cur_n1_node->getName())
      {
         if(cur_n1_node->hasAttribute("interactivedesign"))
         {
            this->interactivedesign = cur_n1_node->getAttribute("General").getValue<bool>();
         }
         if(cur_n1_node->hasAttribute("interactiveGA"))
         {
            this->interactiveGA = cur_n1_node->getAttribute("interactiveGA").getValue<bool>();
         }
        if(cur_n1_node->hasAttribute("interactivedisplay"))
         {
            this->interactivedisplay = cur_n1_node->getAttribute("interactivedisplay").getValue<bool>();
         }
         if(cur_n1_node->hasAttribute("vrservername"))
         {
            this->vrservername = const_cast<char*>(cur_n1_node->getAttribute("vrservername").getString().c_str());
         }
         if(cur_n1_node->hasAttribute("cfdservername"))
         {
            this->cfdservername = const_cast<char*>(cur_n1_node->getAttribute("cfdservername").getString().c_str());
         }
         if(cur_n1_node->hasAttribute("cfdpackage"))
         {
            this->cfdpackage = const_cast<char*>(cur_n1_node->getAttribute("cfdpackage").getString().c_str());
         }

      }
      else if("InteractiveDesign"==cur_n1_node->getName())
      {
         cppdom::NodeList n2 = cur_n1_node->getChildren();
         for(cppdom::NodeList::iterator cur_n2_elt = n2.begin(); cur_n2_elt !=n2.end(); ++ cur_n2_elt)
         {
            cppdom::NodePtr cur_n2_node =*cur_n2_elt;
            if("GAmethod"==cur_n2_node->getName())
            {
               cppdom::NodePtr cur_general_node = cur_n2_node->getChild("General");
               if(cur_general_node->hasAttribute("popsize"))
               {
                  this->popsize = cur_general_node->getAttribute("popsize").getValue<int>();
               }
               
               if(cur_general_node->hasAttribute("totalruns"))
               {
                  this->totalruns = cur_general_node->getAttribute("totalruns").getValue<int>();
               }
               
               if(cur_general_node->hasAttribute("totalmatingevents"))
               {
                  this->totalmatingevents = cur_general_node->getAttribute("totalmatingevents").getValue<int>();
               }
               
               if(cur_general_node->hasAttribute("initialtype"))
               {
                  this->initialtype = cur_general_node->getAttribute("initialtype").getValue<int>();
               }
               
               if(cur_general_node->hasAttribute("boundaryfilename"))
               {
                  this->boundaryfilename = const_cast<char*>(cur_general_node->getAttribute("boundaryfilename").getString().c_str());
               }
               if(cur_general_node->hasAttribute("crossoverrate"))
               {
                  this->crossoverrate = cur_general_node->getAttribute("crossoverrate").getValue<float>();
               }
               if(cur_general_node->hasAttribute("mutationrate"))
               {
                  this->mutationrate = cur_general_node->getAttribute("mutationrate").getValue<float>();
               }
               if(cur_general_node->hasAttribute("numdesignparams"))
               {
                  this->numdesignparams = cur_general_node->getAttribute("numdesignparams").getValue<int>();
               }
               if(cur_general_node->hasAttribute("initialpopfilename"))
               {
                  this->initialpopfilename = const_cast<char*>(cur_general_node->getAttribute("initialpopfilename").getString().c_str());
               }
            }
            
            else if("InteractiveGA"==cur_n2_node->getName())
            {

            }
            else
            {  
               //do nothing
            }

         }
      }
      else if("InteractiveDisplay"==cur_n1_node->getName())
      {

      }
      else
      {
         //do nothing
      }
   }
    
}


char* cfdVeReadParam::getVRserverName()
{
   return this->vrservername;
}

bool cfdVeReadParam::getModeForOldModel()
{
   return this->keepoldmodel;
}


float* cfdVeReadParam::getTransForOldModel()
{
   return this->oldmodeltrans;
}


float* cfdVeReadParam::getRotForOldModel()
{
   return this->oldmodelrot;
}


char* cfdVeReadParam:: getNewVtkFileName()
{
   return this->newvtkfilename;
}


float* cfdVeReadParam::getCurrentDesignParams()
{
   return this->currentdesignparams;
}


int* cfdVeReadParam::getActiveParams()
{
   return this->activeparamsflag;
}

int cfdVeReadParam::getCheckedGene()
{
   return this->checkedgene;
}


float cfdVeReadParam::getCrossOverRate()
{
   return this->crossoverrate;
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
   return this->boundaryfilename;
}

char* cfdVeReadParam::getInitialPopFileName()
{
   return this->initialpopfilename;
}

char* cfdVeReadParam::getDirectoryName()
{
   return this->directory;
}

bool cfdVeReadParam::getInteractiveDisplayFlag()
{
   return this->interactivedisplayflag;
}

int cfdVeReadParam::getChangeType()
{
   return this->changetype;

}

int cfdVeReadParam::getPopSize()
{
   return this->popsize;
}

int cfdVeReadParam::getNumDesignParams()
{
   return this->numdesignparams;
}
void cfdVeReadParam::setSaveMode(bool value)
{
   this->savemode = value;
}

bool cfdVeReadParam::getSaveModel()
{
   return this->savemode;
}

void cfdVeReadParam::setDisplayMode(bool value)
{
   this->displaymode = value;
}

bool cfdVeReadParam::getDisplyMode()
{
   return this->displaymode;
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
   return this->boundaryfile;
}
char* cfdVeReadParam::getInitialPopFileName()
{
   return this->initialpopfile;
}


