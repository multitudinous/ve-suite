// --- My Includes --- //
#include "osgDLL.h"

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgGA/TrackballManipulator>

#include <osgViewer/Viewer>

// --- C/C++ Libraries --- //
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
class KeyboardEventHandler : public osgGA::GUIEventHandler
{
public:
    KeyboardEventHandler( osg::Group* rootNode )
    {
        hMyDll = NULL;

        mRootNode = rootNode;
    }

    virtual bool handle( const osgGA::GUIEventAdapter& ea,
                         osgGA::GUIActionAdapter& )
    {
        switch( ea.getEventType() )
        {
            case( osgGA::GUIEventAdapter::KEYDOWN ):
            {
                if( ea.getKey() == 'l' ) 
                {
                    hMyDll = ::LoadLibrary( "osgDLL.dll" );

                    if( hMyDll != NULL )
                    {
                        //Get the functions address
                        CreateFn pfnCreate = ( CreateFn )GetProcAddress(
                            hMyDll, "CreateOSGGeode" );

                        //Release Dll if we werent able to get the function
                        if( pfnCreate == 0 )   
                        {
                            ::FreeLibrary( hMyDll );
                            return true;
                        }

                        //Call the Function
                        void* pCreate = pfnCreate();

                        //Release if it didnt work
                        if( !pCreate )   
                        {   
                            ::FreeLibrary( hMyDll );
                            return true;
                        }
                        std::cout << "Loaded dll" << std::endl;

                        osg::ref_ptr< osg::Geode > geode =
                            static_cast< osg::Geode* >( pCreate );
                        mRootNode->addChild( geode.get() );
                    }
                }
                else if( ea.getKey() == 'u' )
                {
                    mRootNode->removeChild( 0, 1 );

                    //Release the DLL if we dont have any use for it now
                    ::FreeLibrary( hMyDll );

                    std::cout << "Unloaded dll" << std::endl;
                }

                return true;
            }

            default:
                return false;
        }
    }

private:
    HINSTANCE hMyDll;

    osg::ref_ptr< osg::Group > mRootNode;
};
////////////////////////////////////////////////////////////////////////////////
int main( int argc, char ** argv )
{
    osg::ArgumentParser arguments( &argc, argv );

    osg::ref_ptr< osg::Group > root = new osg::Group();


    osgViewer::Viewer viewer( arguments );
    viewer.setCameraManipulator( new osgGA::TrackballManipulator() );
    viewer.addEventHandler( new KeyboardEventHandler( root.get() ) );
    viewer.setUpViewInWindow( 10, 510, 640, 480 );
    viewer.setSceneData( root.get() );
        
    return viewer.run();
}
////////////////////////////////////////////////////////////////////////////////
