//
// Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
// All rights reserved.
//


#include <wx/wx.h>
#include <wx/cmdline.h>
#include <wx/image.h>
#include <wx/menu.h>
#include <wx/statusbr.h>
#include <wx/treectrl.h>
#include <wx/splitter.h>

#include <osgViewer/ViewerEventHandlers>

#include <osg/NodeVisitor>
#include <osg/Geode>
#include <osg/Group>
#include <osg/LOD>
#include <osg/AnimationPath>
#include <osgGA/TrackballManipulator>
#include <osgDB/ReadFile>

#include <wxpick.h>
#include <osgWxTree/TreeControl.h>
#include <osgWxTree/Utils.h>

#include <iostream>

#ifdef _WIN32
#define random rand
#endif


void randomTree( wxTreeCtrl * tree,
                 unsigned int nn = 1000 )
{
    wxTreeItemId activeGroup_ = osgWxTree::setTreeRoot( tree, new osg::Group );
    for( unsigned int ii = nn; ii > 0; ii-- )
    {
        if( random() / float( RAND_MAX ) < .999 )
        {
            osgWxTree::addTreeItem( tree, activeGroup_, new osg::Geode );
        }
        else
        {
            activeGroup_ = osgWxTree::addTreeItem( tree, activeGroup_, new osg::Group );
        }
    }
}

// `Main program' equivalent, creating windows and returning main app frame
bool wxOsgApp::OnInit()
{
    // copy wxargs to form we can use with osg
    std::vector< std::string > arguments;
    char *argarray[ 100 ];
    for( int ii=0; ii<argc; ii++ )
    {
        std::string argument = std::string( wxString( argv[ ii ] ).mb_str() );
        arguments.push_back( argument );
    }
    for( unsigned int ii = 0; ii < arguments.size(); ++ii )
    {
        argarray[ ii ] = const_cast< char* >( arguments[ ii ].c_str() );
    }
    int argcount = arguments.size();
    
    // parse arguments
    if( argcount < 2 )
    {
        std::cout << arguments[ 0 ] << ": requires filename argument." << std::endl;
        return( false );
    }
    osg::ArgumentParser argparser(&argcount,argarray);
    flat_mode_ = argparser.read( "--flat" );

    int width = 800;
    int height = 600;

    //
    // Create the main osg window
    MainFrame * frame = new MainFrame( NULL, wxT( "Physics Picker" ),
                                      wxDefaultPosition, wxSize( width, height ) );

    // // create a parent frame
    wxSplitterWindow * main_ = new wxSplitterWindow( frame, wxID_ANY );
    wxPanel * tree_panel   = new wxPanel( main_, wxID_ANY );
    wxPanel * render_panel = new wxPanel( main_, wxID_ANY );
    main_->SplitVertically( tree_panel, render_panel );
    main_->Show( true );

    // create tree & edit window
    // wxSplitterWindow * treepropsplit = new wxSplitterWindow( tree_panel, wxID_ANY );
    // treepropsplit->Show( true );

    osgWxTree::TreeControl * tree = new osgWxTree::TreeControl( tree_panel, wxID_ANY, wxDefaultPosition, wxSize(300, height),
       wxTR_DEFAULT_STYLE | wxTR_HAS_BUTTONS | wxTR_ROW_LINES | wxTR_SINGLE );
    tree->Show( true );

    // randomTree( tree, 3e5 ); // tree->ExpandAll();

    // wxPanel *propertypanel = new wxPanel( treepropsplit, wxID_ANY );
    //
    // treepropsplit->SplitHorizontally( tree, propertypanel );


    // create osg canvas
    int * attributes = new int[ 7 ];
    attributes[ 0 ] = int( WX_GL_DOUBLEBUFFER );
    attributes[ 1 ] = WX_GL_RGBA;
    attributes[ 2 ] = WX_GL_DEPTH_SIZE;
    attributes[ 3 ] = 8;
    attributes[ 4 ] = WX_GL_STENCIL_SIZE;
    attributes[ 5 ] = 8;
    attributes[ 6 ] = 0;

    OSGCanvas * canvas = new OSGCanvas( render_panel, wxID_ANY, wxDefaultPosition,
                                        wxSize( width, height ), wxSUNKEN_BORDER, wxT( "osgviewerWX" ), attributes );

    GraphicsWindowWX * gw = new GraphicsWindowWX( canvas );

    canvas->SetGraphicsWindow( gw );

    // load the scene.
    wxString fname( argv[ 1 ] );
    osg::ref_ptr< osg::Group > root = new osg::Group; 
    osg::ref_ptr< osg::Node > loadedModel = osgDB::readNodeFiles( argparser );
    if ( loadedModel.valid() ) root->addChild( loadedModel.get() );

    // construct the viewer
    osgViewer::Viewer * viewer = new osgViewer::Viewer;
    viewer->getCamera()->setGraphicsContext( gw );
    viewer->getCamera()->setViewport( 0, 0, width, height );
    viewer->addEventHandler( new osgViewer::StatsHandler );
    viewer->addEventHandler( new osgWxTree::PickHandler( tree ) );
    viewer->setThreadingModel( osgViewer::Viewer::SingleThreaded );

    // populate the ui control
    if ( !flat_mode_ )
    {
        // create a full tree or
        osgWxTree::PopulateTreeControlWithNodeVisitor pt( tree, true );
        root->accept( pt );
    }
    else
    {
        // create a flattened tree
        osgWxTree::setTreeToFlatGroup( tree, root.get() );
    }

    viewer->setSceneData( root.get() );
    viewer->setCameraManipulator( new osgGA::TrackballManipulator );
    frame->SetViewer( viewer );
    frame->Show( true );

    return( true );
}

static const wxCmdLineEntryDesc cmdlinedesc [] =
{
     { wxCMD_LINE_SWITCH, wxT("h"), wxT("help"), wxT("displays help on the command line parameters"),
          wxCMD_LINE_VAL_NONE, wxCMD_LINE_OPTION_HELP },
     { wxCMD_LINE_SWITCH, wxT("F"), wxT("flat"), wxT("enables flat browser view") },
     { wxCMD_LINE_NONE }
};

void wxOsgApp::OnInitCmdLine( wxCmdLineParser& parser )
{
    parser.SetDesc( cmdlinedesc );
    parser.SetSwitchChars ( wxT("-") );
}

bool wxOsgApp::OnCmdLineParsed( wxCmdLineParser& parser )
{
    // known args
    flat_mode_ = parser.Found( wxT("F") );
    
    // all others
    for ( unsigned int unnamedidx = 0; unnamedidx < parser.GetParamCount(); unnamedidx++ )
    {
        std::string file( parser.GetParam( unnamedidx ).mb_str() );
        files_.push_back( file );
    }
    return true;
}

IMPLEMENT_APP( wxOsgApp )

BEGIN_EVENT_TABLE( MainFrame, wxFrame )
EVT_IDLE( MainFrame::OnIdle )
// EVT_TREE_ITEM_EXPANDING( )
END_EVENT_TABLE()

/* My frame constructor */
MainFrame::MainFrame( wxFrame * frame, const wxString & title, const wxPoint & pos,
                      const wxSize & size, long style )
    : wxFrame( frame, wxID_ANY, title, pos, size, style )
{
    statusbar_ = this->CreateStatusBar();
    statusbar_->SetStatusText( wxT( "we\'ll display the full-picked node types here" ) );

    // WXTREECTRL

}

void MainFrame::SetViewer( osgViewer::Viewer * viewer )
{
    viewer_ = viewer;
}

void MainFrame::OnIdle( wxIdleEvent & event )
{
    viewer_->frame();

    event.RequestMore();
}

BEGIN_EVENT_TABLE( OSGCanvas, wxGLCanvas )
EVT_SIZE                ( OSGCanvas::OnSize )
EVT_PAINT               ( OSGCanvas::OnPaint )
EVT_ERASE_BACKGROUND    ( OSGCanvas::OnEraseBackground )

EVT_CHAR                ( OSGCanvas::OnChar )
EVT_KEY_UP              ( OSGCanvas::OnKeyUp )

EVT_ENTER_WINDOW        ( OSGCanvas::OnMouseEnter )
EVT_LEFT_DOWN           ( OSGCanvas::OnMouseDown )
EVT_MIDDLE_DOWN         ( OSGCanvas::OnMouseDown )
EVT_RIGHT_DOWN          ( OSGCanvas::OnMouseDown )
EVT_LEFT_UP             ( OSGCanvas::OnMouseUp )
EVT_MIDDLE_UP           ( OSGCanvas::OnMouseUp )
EVT_RIGHT_UP            ( OSGCanvas::OnMouseUp )
EVT_MOTION              ( OSGCanvas::OnMouseMotion )
END_EVENT_TABLE()

OSGCanvas::OSGCanvas( wxWindow * parent, wxWindowID id,
                      const wxPoint & pos, const wxSize & size, long style, const wxString & name, int * attributes )
    : wxGLCanvas( parent, id, pos, size, style | wxFULL_REPAINT_ON_RESIZE, name, attributes )
{
    // default cursor to standard
    _oldCursor = *wxSTANDARD_CURSOR;
}

OSGCanvas::~OSGCanvas()
{
}

void OSGCanvas::OnPaint( wxPaintEvent & WXUNUSED( event ) )
{
    /* must always be here */
    wxPaintDC dc( this );
}

void OSGCanvas::OnSize( wxSizeEvent & event )
{
    // this is also necessary to update the context on some platforms
    wxGLCanvas::OnSize( event );

    // set GL viewport (not called by wxGLCanvas::OnSize on all platforms...)
    int width, height;
    GetClientSize( &width, &height );

    if( _graphics_window.valid() )
    {
        // update the window dimensions, in case the window has been resized.
        _graphics_window->getEventQueue()->windowResize( 0, 0, width, height );
        _graphics_window->resized( 0, 0, width, height );
    }
}

void OSGCanvas::OnEraseBackground( wxEraseEvent & WXUNUSED( event ) )
{
    /* Do nothing, to avoid flashing on MSW */
}

void OSGCanvas::OnChar( wxKeyEvent & event )
{
#if wxUSE_UNICODE
    int key = event.GetUnicodeKey();
#else
    int key = event.GetKeyCode();
#endif

    if( _graphics_window.valid() )
        _graphics_window->getEventQueue()->keyPress( key );

    // If this key event is not processed here, we should call
    // event.Skip() to allow processing to continue.
}

void OSGCanvas::OnKeyUp( wxKeyEvent & event )
{
#if wxUSE_UNICODE
    int key = event.GetUnicodeKey();
#else
    int key = event.GetKeyCode();
#endif

    if( _graphics_window.valid() )
        _graphics_window->getEventQueue()->keyRelease( key );

    // If this key event is not processed here, we should call
    // event.Skip() to allow processing to continue.
}

void OSGCanvas::OnMouseEnter( wxMouseEvent & event )
{
    // Set focus to ourselves, so keyboard events get directed to us
    SetFocus();
}

void OSGCanvas::OnMouseDown( wxMouseEvent & event )
{
    if( _graphics_window.valid() )
    {
        _graphics_window->getEventQueue()->mouseButtonPress( event.GetX(), event.GetY(),
                                                            event.GetButton() );
    }
}

void OSGCanvas::OnMouseUp( wxMouseEvent & event )
{
    if( _graphics_window.valid() )
    {
        _graphics_window->getEventQueue()->mouseButtonRelease( event.GetX(), event.GetY(),
                                                              event.GetButton() );
    }
}

void OSGCanvas::OnMouseMotion( wxMouseEvent & event )
{
    if( _graphics_window.valid() )
        _graphics_window->getEventQueue()->mouseMotion( event.GetX(), event.GetY() );
}

void OSGCanvas::UseCursor( bool value )
{
    if( value )
    {
        // show the old cursor
        SetCursor( _oldCursor );
    }
    else
    {
        // remember the old cursor
        _oldCursor = GetCursor();

        // hide the cursor
        //    - can't find a way to do this neatly, so create a 1x1, transparent image
        wxImage image( 1, 1 );
        image.SetMask( true );
        image.SetMaskColour( 0, 0, 0 );
        wxCursor cursor( image );
        SetCursor( cursor );

        // On wxGTK, only works as of version 2.7.0
        // (http://trac.wxwidgets.org/ticket/2946)
        // SetCursor( wxStockCursor( wxCURSOR_BLANK ) );
    }
}

GraphicsWindowWX::GraphicsWindowWX( OSGCanvas * canvas )
{
    _canvas = canvas;

    _traits = new GraphicsContext::Traits;

    wxPoint pos = _canvas->GetPosition();
    wxSize size = _canvas->GetSize();

    _traits->x = pos.x;
    _traits->y = pos.y;
    _traits->width = size.x;
    _traits->height = size.y;

    init();
}

GraphicsWindowWX::~GraphicsWindowWX()
{
}

void GraphicsWindowWX::init()
{
    if( valid() )
    {
        setState( new osg::State );
        getState()->setGraphicsContext( this );

        if( _traits.valid() && _traits->sharedContext )
        {
            getState()->setContextID( _traits->sharedContext->getState()->getContextID() );
            incrementContextIDUsageCount( getState()->getContextID() );
        }
        else
        {
            getState()->setContextID( osg::GraphicsContext::createNewContextID() );
        }
    }
}

void GraphicsWindowWX::grabFocus()
{
    // focus the canvas
    _canvas->SetFocus();
}

void GraphicsWindowWX::grabFocusIfPointerInWindow()
{
    // focus this window, if the pointer is in the window
    wxPoint pos = wxGetMousePosition();
    if( wxFindWindowAtPoint( pos ) == _canvas )
        _canvas->SetFocus();
}

void GraphicsWindowWX::useCursor( bool cursorOn )
{
    _canvas->UseCursor( cursorOn );
}

bool GraphicsWindowWX::makeCurrentImplementation()
{
    _canvas->SetCurrent();
    return( true );
}

void GraphicsWindowWX::swapBuffersImplementation()
{
    _canvas->SwapBuffers();
}

