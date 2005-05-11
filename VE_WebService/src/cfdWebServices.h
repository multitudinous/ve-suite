#ifndef CFD_WEBSERVICE_H
#define CFD_WEBSERVICE_H

#include "interface.h"

#include <map>
#include <string>

#define USE_TEST_FILE

class cfdDCS;
class cfdGroup;
class cfdGauges;
class cfdDashboard;
class cfdExecutiveConfiguration;
class cfdInteractiveGeometry;
class veWebService_i;
class cfdCommandArray;
class Network;
class cfdVEAvailModules;
class cfdVEBaseClass;
class cfdVEAvail_Modules;
class cfdVjObsWrapper;
class cfdThread;

namespace Body { class Executive; }
namespace CosNaming { class NamingContext; }
namespace PortableServer { class POA; }

class cfdWebServices 
{
   public:
      cfdWebServices( CosNaming::NamingContext*, PortableServer::POA* );

      ~cfdWebServices( void );

      void InitOrbNaming( void );

      // the Computational Engine
      Body::Executive* exec;

      CosNaming::NamingContext* namingContext;

      // _id_map : maps a module id to an interface object for a module's inputs.
      std::map<int, Interface>   interfaceMap;
  
      // _pt_map : maps a module id to an interface object for a module'ss port data.
      std::map<int, Interface>   portMap;
  
      // _ot_map : maps a module id to an interface object for a modules's outputs.
      //std::map<int, Interface>   _ot_map;
  
      // _name_map : maps a module id to its module name.
      std::map< int, std::string> IDMap;
      std::map< std::string, int > nameMap;
  
      //maps a module's ID to its instance
      std::map<int, cfdVEBaseClass* > pluginMap;

      // Functions that operate on the Executive
      char* GetNetwork( void );
     // void GetOutput( std::string name);
    //  void GetPort( std::string name);
      void GetEverything( void );
      //void HowToUse( std::string name);

      // Get intial module information from the executive
      void InitModules( void );

      // Update function called from within preFrame
      void UpdateModules( void );
   
      // Function called within preFrame to allow cfdExecutive
      // to have access to scalar information
      void UnbindORB( void );

      void SetCalculationsFlag( bool );

      bool GetCalculationsFlag( void );

      // compare VjObs_i commandArray with its child's value
      //virtual bool CheckCommandId( cfdCommandArray* );

      // in future, multi-threaded apps will make a copy of VjObs_i commandArray
      //virtual void UpdateCommand();

      //Loading the Available Modules
      cfdVEAvail_Modules* availableModules; 

      // Network class to decode network string
      Network* network;
   private:
      
      void insertItemIntoSQL(Interface &interface);   //insert the data for a
                                       //particular interface into MYSQL
      cfdExecutiveConfiguration* param;
      std::string activeScalarName;
      cfdGauges* gauges;
      cfdDashboard* dashBoard;
      cfdInteractiveGeometry* geometry;
      veWebService_i* uii;
      cfdGroup* masterNode;

      bool doneWithCalculations;
      bool updateNetworkString;
      bool runGetEverythingThread;
      FILE* outputFile;
      struct StringHolder
      {
         int ID;
         std::string intString;
         std::string doubleString;      
         std::string stringString;
         std::string intArrayString;
         std::string doubleArrayString;      
         std::string stringArrayString;
      };

};

#endif

