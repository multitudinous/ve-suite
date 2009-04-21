
// --- VE-Suite Includes --- //
#include "DBApp.h"
#include "AppFrame.h"

#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/cad/CADCreator.h>

// --- Xerces Includes --- //
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

// --- wxWidgets Includes --- //
#include <wx/confbase.h>

XERCES_CPP_NAMESPACE_USE

IMPLEMENT_APP( DBApp );

////////////////////////////////////////////////////////////////////////////////
bool DBApp::OnInit()
{
    //Initialize Xerces
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch( const XMLException& toCatch )
    {
        XERCES_STD_QUALIFIER cerr
            << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode( toCatch.getMessage() )
            << XERCES_STD_QUALIFIER endl;

        return false;
    }

    //Initialize VE-Open
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "XML", new ves::open::xml::XMLCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "Shader", new ves::open::xml::shader::ShaderCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "Model", new ves::open::xml::model::ModelCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "CAD", new ves::open::xml::cad::CADCreator() );

    //Set the app name
    SetAppName( wxT( "VE-DB" ) );

    //Create the app frame
    m_appFrame = new AppFrame( NULL, wxNewId() );
    m_appFrame->Show( true );
    SetTopWindow( m_appFrame );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
int DBApp::OnExit()
{
    delete wxConfigBase::Set( ( wxConfigBase* ) NULL );

    XMLPlatformUtils::Terminate();

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
