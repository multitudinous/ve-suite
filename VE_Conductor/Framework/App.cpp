#include "App.h"
#include "Frame.h"
#include "package.h"
IMPLEMENT_APP(REIApp);

bool REIApp::OnInit()
{

  try
    {
      XMLPlatformUtils::Initialize();
    }
  
  catch(const XMLException &toCatch)
    {
      XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
				<< "  Exception message:"
				<< XMLString::transcode(toCatch.getMessage()) << XERCES_STD_QUALIFIER endl;
      return 1;
    }


  SetAppName("VE-Conductor");
   
  mainFrame= new AppFrame((wxFrame*) NULL, 1023, "VE-Conductor");
 
  // Problem with generic wxNotebook implementation whereby it doesn't size
  // properly unless you set the size again

#if defined(__WIN16__) || defined(__WXMOTIF__)
   int width, height;
	mainFrame->GetSize(& width, & height);
	mainFrame->SetSize(-1, -1, width, height);
#endif
  

   
   mainFrame->Show(true);
   SetTopWindow(mainFrame);
   return true;

}

int REIApp::OnExit()
{
  XMLPlatformUtils::Terminate();
  return 0;
}
