//***insert license here***\\

//#include "cfdAppWrapper.h"
//#include "cfdVjObsWrapper.h"

//This class manages the VE Suite web interface
#ifdef _TAO
//#include "Executive_i.h"
#include <orbsvcs/CosNamingC.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#else
#include <omniORB4/CORBA.h>
#endif
#include <iostream>

class cfdWebServices : public cfdGlobalBase
{
   public:
      cfdWebServices();
      ~cfdWebServices();
      bool initCorba(int&, char**);
   
   private:

      CosNaming::NamingContext* _naming_context;
      // _it_map : maps a module id to an interface object for a module's inputs.
      std::map<int, Interface>   _it_map;
  
      // _pt_map : maps a module id to an interface object for a module'ss port data.
      std::map<int, Interface>   _pt_map;
  
      // _ot_map : maps a module id to an interface object for a modules's outputs.
      //std::map<int, Interface>   _ot_map;
  
      // _name_map : maps a module id to its module name.
      std::map< int, std::string> _id_map;
      std::map< std::string, int > _name_map;
  
      // _name_map : maps a module name to its module id.
      std::map<int, cfdVEBaseClass* > _plugins;
      
      cfdGroup* _masterNode;s
      CORBA::ORB-var _orb;           //our CORBA orb.  maybe this can be a
                                    // temp variable in the initCorba function,
                                    //but will I know that for sure, it's here
};
