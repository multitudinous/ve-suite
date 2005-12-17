#ifndef DOMDOCUMENT_MANAGER
#define DOMDOCUMENT_MANAGER
#include "VE_Installer/include/VEConfig.h"

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

#include <string>
XERCES_CPP_NAMESPACE_USE
namespace VE_Conductor
{
// This class should be made into a singleton
class VE_CONDUCTOR_EXPORTS DOMDocumentManager
{
public: 
   DOMDocumentManager( void );
   ~DOMDocumentManager( void );

   // Runs the intialization routines for xerces
   void InitializeXerces( void );

   // Writes the document that is passed in and returns a string
   std::string WriteDocumentToString( DOMDocument* document );

   // Get the respective documents for each portion of VE-Suite
   DOMDocument* GetCommandDocument( void );
   DOMDocument* GetParameterDocument( void );
   DOMDocument* GetModulesDocument( void );

   void Load( const std::string inputCommand );
   void UnLoadParser( void );

   // Functions used to create a document and then return it in a std::string
   std::string WriteAndReleaseCommandDocument( void );
   void CreateCommandDocument( void );

private:
   // This is the document used ot send commands to Xplorer
   DOMDocument* commandDocument;
   // This is the document used to send parameter file info
   // to Xplorer
   DOMDocument* parameterDocument;
   // This is the current nt file sent to the CE and is
   // constructed by Conductor
   DOMDocument* modulesDocument;

   XercesDOMParser* parser;
   ErrorHandler* errHandler;
};
}
#endif
