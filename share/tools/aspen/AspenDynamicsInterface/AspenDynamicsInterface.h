#ifndef ASPENDYNAMICSINTERFACE_H
#define ASPENDYNAMICSINTERFACE_H

#include <set>
#include <vector>
#include <map>


#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdisp.h>        // MFC Automation classes
#include <afxdtctl.h>		// MFC support for Internet Explorer 4 Common Controls
#include <afxcmn.h>			// MFC support for Windows Common Controls


#  ifdef ASPENDYNAMICSINTERFACE_LIBRARY
#    define ASPENDYNAMICSINTERFACE_EXPORTS   __declspec(dllexport)
#  else
#    define ASPENDYNAMICSINTERFACE_EXPORTS   __declspec(dllimport)
#  endif // DASI_EXPORTS

#import "aspendynamics.tlb"

namespace AspenDynamicsInterface
{
	class ASPENDYNAMICSINTERFACE_EXPORTS AspenDynamicsInterface
	{
	
		public:
		AspenDynamicsInterface();
		~AspenDynamicsInterface();
	
		//File operating functions
        //Open an Aspen Document
		void Open(CString filename);
        //Close the file, clear up 
		void Close();
		void Quit();
        //Save the document back;
		void Save();
        //save this as another document
		void SaveAs(CString filename);
		void SetVisibility( bool status );
        void RunSolver( );
        void ResetSimulation( );
        std::vector< std::vector< std::string > > GetVariableList( CString itemName, bool block );
        void SetVariableValue( CString itemName, CString variableName,
            CString Value );


        private:
		bool simOpened;
        AspenDynamicsLibrary::IAspenModelerPtr ADApplication;
        AspenDynamicsLibrary::IAspenModelerDocumentPtr ADDocument;
        AspenDynamicsLibrary::IAspenModelerPhysicalPropertiesPtr ADProperties;
        AspenDynamicsLibrary::IAspenModelerResultsPtr ADResults;
	};
}
#endif
