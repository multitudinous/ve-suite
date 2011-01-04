/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/UIPluginBase.h>
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/util/DataSetLoaderUI.h>

#include <iostream>

#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>

#include <ves/conductor/SummaryResultDialog.h>
#include <ves/conductor/UIDialog.h>
#include <ves/conductor/TextResultDialog.h>
#include <ves/conductor/TexTable.h>
#include <ves/conductor/IconChooser.h>
#include <ves/conductor/paraThread.h>
#include <ves/conductor/vistab.h>
#include <ves/conductor/AspenPlus2DIcons.h>

#include <ves/conductor/util/OrbThread.h>
#include <ves/conductor/util/ParamsDlg.h>
#include <ves/conductor/util/AspenDynamicsDialog.h>
#include <ves/conductor/util/SoundsPane.h>

// EPRI TAG
#include <ves/conductor/FinancialDialog.h>
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/OneDStringArray.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/model/SystemPtr.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/model/NetworkPtr.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Port.h>

#include <ves/conductor/util/CADNodeManagerDlg.h>
#include <ves/conductor/xpm/contour.xpm>
//#include <ves/conductor/xpm/cad_tree_selected.xpm>
//#include <ves/conductor/xpm/cad_tree_unselected.xpm>
//#include <ves/conductor/xpm/cspline.xpm>
#include <ves/conductor/xpm/isosurface.xpm>
#include <ves/conductor/xpm/ROItb.xpm>
#include <ves/conductor/xpm/square.xpm>
#include <ves/conductor/xpm/streamlines.xpm>
#include <ves/conductor/xpm/vector.xpm>
#include <ves/conductor/xpm/vectortb.xpm>

#include <wx/dc.h>
#include <wx/dcbuffer.h>
#include <wx/msgdlg.h>
#include <wx/image.h>
#include <wx/wx.h>

#include <cmath>
#include <wx/msgdlg.h>

#include <fstream>

using namespace ves::open::xml::model;
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( UIPluginBase, wxEvtHandler )
    EVT_LEFT_DCLICK( UIPluginBase::OnDClick )
    EVT_RIGHT_DOWN( UIPluginBase::OnMRightDown )
    EVT_MENU( UIPLUGINBASE_SHOW_RESULT, UIPluginBase::OnShowResult )
    EVT_MENU( UIPLUGINBASE_PARAVIEW, UIPluginBase::OnParaView )
    EVT_MENU( UIPLUGINBASE_SHOW_DESC, UIPluginBase::OnShowDesc )
    EVT_MENU( UIPLUGINBASE_USER_DIALOG, UIPluginBase::OnShowUserDialog )
    EVT_MENU( UIPLUGINBASE_SHOW_FINANCIAL, UIPluginBase::OnShowFinancial ) /* EPRI TAG */
    //EVT_MENU( SHOW_ASPEN_NAME, UIPluginBase::OnShowAspenName )
    //EVT_MENU( QUERY_DYNAMICS, UIPluginBase::OnQueryDynamics )
    //EVT_MENU( QUERY_INPUTS, UIPluginBase::OnQueryInputs )
    //EVT_MENU( QUERY_OUTPUTS, UIPluginBase::OnQueryOutputs )
    //EVT_MENU( REINIT_BLOCK, UIPluginBase::OnReinitBlocks )
    EVT_MENU( UIPLUGINBASE_SHOW_ICON_CHOOSER, UIPluginBase::OnShowIconChooser )
    EVT_MENU( UIPLUGINBASE_GEOMETRY, UIPluginBase::OnGeometry )
    EVT_MENU( UIPLUGINBASE_NAVTO, UIPluginBase::OnNavigateTo )
    EVT_MENU( UIPLUGINBASE_NAVTO_SELECT, UIPluginBase::OnNavigateTo )
    EVT_MENU( UIPLUGINBASE_OPTIMIZE_CAD, UIPluginBase::OnOptimizeCAD )
    EVT_MENU( UIPLUGINBASE_DATASET, UIPluginBase::OnDataSet )
    EVT_MENU( UIPLUGINBASE_MODEL_INPUTS, UIPluginBase::OnInputsWindow ) /* EPRI TAG */
    EVT_MENU( UIPLUGINBASE_MODEL_RESULTS, UIPluginBase::OnResultsWindow ) /* EPRI TAG */
    EVT_MENU( UIPLUGINBASE_VISUALIZATION, UIPluginBase::OnVisualization )
    EVT_MENU( UIPLUGINBASE_SET_UI_PLUGIN_NAME, UIPluginBase::OnSetUIPluginName )
    EVT_MENU( UIPLUGINBASE_SET_ACTIVE_MODEL, UIPluginBase::OnSetActiveXplorerModel )
    EVT_MENU( UIPLUGINBASE_ACTIVE_MODEL_SOUNDS, UIPluginBase::OnModelSounds )
    EVT_MENU( UIPLUGINBASE_DEL_MOD, UIPluginBase::OnDelMod )
    EVT_MENU( UIPLUGINBASE_ZOOM, UIPluginBase::OnZoomSelected )
    EVT_MENU( UIPLUGINBASE_MAKE_HIER, UIPluginBase::OnMakeIntoHierarchy )
    EVT_MENU( UIPLUGINBASE_ADD_INPUT_PORT, UIPluginBase::AddPort )
    EVT_MENU( UIPLUGINBASE_ADD_OUTPUT_PORT, UIPluginBase::AddPort )
    EVT_MENU( UIPLUGINBASE_DELETE_PORT, UIPluginBase::DeletePort )
    EVT_MENU( UIPLUGINBASE_TOGGLE_ALL_ON, UIPluginBase::TogglePlugin )
    EVT_MENU( UIPLUGINBASE_TOGGLE_PLUGIN_ON, UIPluginBase::TogglePlugin )
    EVT_UPDATE_UI( UIPLUGINBASE_SET_ACTIVE_PLUGIN, UIPluginBase::OnSetActivePluginID )
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS( UIPluginBase, wxEvtHandler )

/////////////////////////////////////////////////////////////////////////////
UIPluginBase::UIPluginBase() :
        wxEvtHandler(),
        financial_dlg( 0 ),
        dlg( 0 ),
        result_dlg( 0 ),
        port_dlg( 0 ),
        m_dataSetLoaderDlg( 0 ),
        m_veModel( new Model() ),
        resultsDialog( 0 ),
        portsDialog( 0 ),
        inputsDialog( 0 ),
        serviceList( 0 ),
        mDataBufferEngine( 0 ),
        mUserPrefBuffer( 0 ),
        iconFilename( "DefaultPlugin" ),
        m_canvas( 0 ),
        m_network( 0 ),
        _soundsDlg( 0 ),
        m_iconChooser( 0 ),
        vistab( 0 ),        
        cadDialog( 0 ),
        highlightFlag( false ),
        nameFlag( true ),
        mPopMenu( 0 )
{
    pos = wxPoint( 0, 0 ); //default position

    mPluginName = wxString( "DefaultPlugin", wxConvUTF8 );
    mDescription = wxString( "Default Plugin", wxConvUTF8 );

    wxImage my_img( square_xpm );
    mIconW = static_cast< int >( my_img.GetWidth() );//*0.30f );
    mIconH = static_cast< int >( my_img.GetHeight() );//*0.30f );
    //mMyIcon=new wxBitmap(my_img.Scale(mIconW, mIconH));
    mMyIcon = new wxBitmap( my_img );

    n_pts = 4;
    poly = new wxPoint[n_pts];
    poly[0] = wxPoint( 0, 0 );
    poly[1] = wxPoint( mIconW - 1, 0 );
    poly[2] = wxPoint( mIconW - 1, mIconH - 1 );
    poly[3] = wxPoint( 0, mIconH - 1 );

    defaultIconMap[ "contour.xpm" ] = wxImage( contour_xpm );
    defaultIconMap[ "isosurface.xpm" ] = wxImage( isosurface_xpm );
    defaultIconMap[ "ROItb.xpm" ] = wxImage( ROItb_xpm );
    defaultIconMap[ "streamlines.xpm" ] = wxImage( streamlines_xpm );
    defaultIconMap[ "vector.xpm" ] = wxImage( vector_xpm );
    defaultIconMap[ "vectortb.xpm" ] = wxImage( vectortb_xpm );
    
    pluginDeleteEvent.SetId( UIPLUGINBASE_DIALOG_PLUGIN_UPDATE );
}
////////////////////////////////////////////////////////////////////////////////
UIPluginBase::~UIPluginBase()
{
    //if( !mDialogMemoryMap.empty() )
    {
        DisconnectPluginDialogsDestroyEvent( dlg );
        DisconnectPluginDialogsDestroyEvent( result_dlg );
        DisconnectPluginDialogsDestroyEvent( port_dlg );
        DisconnectPluginDialogsDestroyEvent( m_dataSetLoaderDlg );
        DisconnectPluginDialogsDestroyEvent( resultsDialog );
        DisconnectPluginDialogsDestroyEvent( portsDialog );
        DisconnectPluginDialogsDestroyEvent( inputsDialog );
        DisconnectPluginDialogsDestroyEvent( _soundsDlg );
        DisconnectPluginDialogsDestroyEvent( m_iconChooser );
        DisconnectPluginDialogsDestroyEvent( vistab );
        DisconnectPluginDialogsDestroyEvent( cadDialog );
    }

    delete [] poly;
    poly = 0;
    
    delete mMyIcon;
    mMyIcon = 0;

    if( result_dlg != NULL )
    {
        delete result_dlg;
        result_dlg = 0;
    }

    if( port_dlg != NULL )
    {
        delete port_dlg;
        port_dlg = 0;
    }

    // EPRI TAG
    if( financial_dlg != NULL )
    {
        delete financial_dlg;
        financial_dlg = 0;
    }

    if( portsDialog )
    {
        portsDialog->Destroy();
        portsDialog = 0;
    }
    
    m_canvas = 0;
    m_network = 0;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetCanvas( wxScrolledWindow* canvas )
{
    m_canvas = dynamic_cast< Canvas * >(canvas);
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetNetwork( ves::conductor::Network* network )
{
    m_network = network;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetID( int id )
{
    this->id = id;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetName( wxString pluginName )
{
    mPluginName = pluginName;
    if( mPopMenu )
    {    
        wxString menuName = mPluginName + wxString( " Menu", wxConvUTF8 );
        mPopMenu->SetTitle( menuName );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetPos( wxPoint pt )
{
    pos = pt;
}
////////////////////////////////////////////////////////////////////////////////
double UIPluginBase::GetVersion()
{
    return 0.1;
}
////////////////////////////////////////////////////////////////////////////////
wxRect UIPluginBase::GetBBox()
{
    wxRect result;
    //int left, right, top, bottom;
    //int i;

    //Calculate the Bounding box out the of polygon
    result.SetX( pos.x );
    result.SetY( pos.y );

    if( n_pts != 4 )
    {
        std::cout << "Could have a problem in icon code." << std::endl;
    }

    int left = poly[0].x;
    int right = poly[1].x;
    int top = poly[1].y;
    int bottom = poly[2].y;

    result.SetWidth( right - left );
    result.SetHeight( bottom - top );
    return result;
}
////////////////////////////////////////////////////////////////////////////////
int UIPluginBase::GetNumPoly( void )
{
    return n_pts;
}
////////////////////////////////////////////////////////////////////////////////
/*ves::conductor::util::Polygon* UIPluginBase::GetPolygon( void )
{
    return &( poly );
}*/
////////////////////////////////////////////////////////////////////////////////
int UIPluginBase::GetNumIports()
{
    return inputPort.size();
}
////////////////////////////////////////////////////////////////////////////////
PORT UIPluginBase::GetIPorts()
{
    return inputPort;
    
    //for( size_t i = 0; i <  inputPort.size(); ++i )
    {

        //iports[ i ] = ( *inputPort.at( i ) );
        /*
         iports[ i ].GetPortLocation()->SetPoint( 
                  std::pair< unsigned int, unsigned int >( poly[ 0 ].x, ( poly[ 3 ].y / inputPort.size() ) * i ) 
                                                );
         inputPort.at( i )->GetPortLocation()->SetPoint( 
                  std::pair< unsigned int, unsigned int >( poly[ 0 ].x, ( poly[ 3 ].y / inputPort.size() ) * i ) 
                                               );
                                    */
        //iports[ i ].x = poly[ 0 ].x;
        //iports[ i ].y = (poly[ 3 ].y / inputPort.size() ) * i;
        //iports[ i ].x = inputPort.at( i )->GetPortLocation()->GetPoint().first;
        //iports[ i ].y = inputPort.at( i )->GetPortLocation()->GetPoint().second;
    }
}
/////////////////////////////////////////////////////////////////////////////
int UIPluginBase::GetNumOports()
{
    return outputPort.size();
}
/////////////////////////////////////////////////////////////////////////////
PORT UIPluginBase::GetOPorts()
{
    return outputPort;
    //for( size_t i = 0; i < outputPort.size(); ++i )
    {
        //oports[ i ] = ( *outputPort.at( i ) );
        /*
         oports[ i ].GetPortLocation()->SetPoint( 
                  std::pair< unsigned int, unsigned int >( poly[ 1 ].x, ( poly[ 3 ].y / outputPort.size() ) * i ) 
                                                );
         outputPort.at( i )->GetPortLocation()->SetPoint( 
                  std::pair< unsigned int, unsigned int >( poly[ 1 ].x, ( poly[ 3 ].y / outputPort.size() ) * i ) 
                                                );
                                     */
        //oports[ i ].x = outputPort.at( i )->GetPortLocation()->GetPoint().first;
        //oports[ i ].y = outputPort.at( i )->GetPortLocation()->GetPoint().second;
    }
}

/////////////////////////////////////////////////////////////////////////////
void UIPluginBase::DrawIcon( wxDC* dc )
{
    dc->DrawBitmap( *mMyIcon, pos.x, pos.y, true );
}

/////////////////////////////////////////////////////////////////////////////
void UIPluginBase::DrawID( wxDC* dc )
{
    ///Need if statement to turn this off and on
    ///Right now it is permenantly turned off
    return; // no module id
    int x, y;
    int w, h;
    wxCoord xoff = pos.x;
    wxCoord yoff = pos.y;

    wxString text;

    x = 0;
    y = 0;

    for( int i = 0; i < n_pts; i++ )
    {
        x += poly[i].x;
        y += poly[i].y;
    }
    x = x / n_pts;
    y = y / n_pts;

    //text<<mod_pack._id;
    dc->GetTextExtent( text, &w, &h );
    dc->DrawText( text, ( x - w / 2 + xoff ), ( y - h / 2 + yoff ) );
}
/////////////////////////////////////////////////////////////////////////////
void UIPluginBase::DrawName( wxDC* dc )
{
    int x = 0;
    int y = 0;
    int w, h;

    wxCoord xoff = pos.x;
    //wxCoord yoff = pos.y;

    for( int i = 0; i < n_pts; ++i )
    {
        x += poly[ i ].x;
        y += poly[ i ].y;
    }

    x = x / n_pts;
    y = y / n_pts;

    dc->GetTextExtent( mPluginName, &w, &h );
    dc->DrawText( mPluginName, int( x - w / 2 + xoff ), pos.y + int( y * 2.1 ) );
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* UIPluginBase::UI( wxWindow* parent )
{
    if( dlg )
    {
        return dlg;
    }

    long new_id = wxNewId();
    dlg = new UIDialog( parent, new_id, _( "UIDialog" ) );
    ConfigurePluginDialogs( dlg );
    
    return dlg;
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* UIPluginBase::Result( wxWindow* parent )
{
    std::vector<wxString> titles;
    //std::vector<std::string> descs;
    std::vector<int> alignments;

    titles.push_back( wxString( "Description", wxConvUTF8 ) );
    alignments.push_back( wxALIGN_LEFT );
    titles.push_back( wxString( "Value", wxConvUTF8 ) );
    alignments.push_back( wxALIGN_RIGHT );

    if( result_dlg == NULL )
        result_dlg = new TextResultDialog( parent, wxT( "Result Summary" ), wxSize( 560, 400 ) );
    result_dlg->syngas->Clear();
    result_dlg->syngas->SetColTitles( titles );
    result_dlg->syngas->SetColAlignments( alignments );
    result_dlg->Set2Cols( v_desc, v_value );
    return result_dlg;
}

/////////////////////////////////////////////////////////////////////////////
unsigned int UIPluginBase::GetID()
{
    return id;
}
/////////////////////////////////////////////////////////////////////////////
wxString UIPluginBase::GetConductorName()
{
    return wxString( "PleaseDefineConductorName", wxConvUTF8 );
}
/////////////////////////////////////////////////////////////////////////////
wxString UIPluginBase::GetName()
{
    if( mPluginName.IsEmpty() )
    {
        mPluginName = wxString( "PleaseDefineClassName", wxConvUTF8 );
    }

    return mPluginName;
}
/////////////////////////////////////////////////////////////////////////////
wxString UIPluginBase::GetHelp()
{
    return _T( "www.vesuite.org" );
}
/////////////////////////////////////////////////////////////////////////////
wxString UIPluginBase::GetDesc()
{
    return mDescription;
}
/////////////////////////////////////////////////////////////////////////////
void UIPluginBase::Lock( bool lock )
{
    if( dlg != NULL )
        dlg->Lock( lock );
}
////////////////////////////////////////////////////////////////////////////////
bool UIPluginBase::Has3Ddata()
{
    return false;
}
////////////////////////////////////////////////////////////////////////////////
ModelPtr UIPluginBase::GetVEModel( void )
{
    if( mPluginName.IsEmpty() )
    {
        mPluginName = wxString( "PleaseDefineClassName", wxConvUTF8 );
    }

    m_veModel->SetPluginName( ConvertUnicode( mPluginName.c_str() ) );
    m_veModel->SetPluginType( m_pluginType.c_str() );
    m_veModel->SetModelID( id );
    m_veModel->SetIconFilename( iconFilename );
    m_veModel->GetIconLocation()->SetPoint( std::pair< unsigned int, unsigned int >( pos.x, pos.y ) );

    {
        ///Set the int data
        std::map<std::string, long *>::iterator iteri;
        for( iteri = _int.begin(); iteri != _int.end(); iteri++ )
        {
                CommandPtr tempCommand = CommandPtr( new Command() );
                tempCommand->SetCommandName( iteri->first );
                ves::open::xml::DataValuePairPtr dataDVP( new ves::open::xml::DataValuePair() );
                dataDVP->SetData( iteri->first, *( iteri->second ) );
                tempCommand->AddDataValuePair( dataDVP );
                m_veModel->SetInput( tempCommand );
        }
    }

    {
        ///Set the double data
        std::map<std::string, double *>::iterator iterd;
        for( iterd = _double.begin(); iterd != _double.end(); iterd++ )
        {
                CommandPtr tempCommand = CommandPtr( new Command() );
                tempCommand->SetCommandName( iterd->first );
                ves::open::xml::DataValuePairPtr dataDVP( new ves::open::xml::DataValuePair() );
                dataDVP->SetData( iterd->first, *( iterd->second ) );
                tempCommand->AddDataValuePair( dataDVP );
                m_veModel->SetInput( tempCommand );
        }
    }

    {
        ///Set the string data
        std::map<std::string, std::string *>::iterator iters;
        for( iters = _string.begin(); iters != _string.end(); iters++ )
        {
                CommandPtr tempCommand = CommandPtr( new Command() );
                tempCommand->SetCommandName( iters->first );
                ves::open::xml::DataValuePairPtr dataDVP( new ves::open::xml::DataValuePair() );
                dataDVP->SetData( iters->first, *( iters->second ) );
                tempCommand->AddDataValuePair( dataDVP );
                m_veModel->SetInput( tempCommand );
        }
    }

    {
        ///Set the 1d int data
        std::map<std::string, std::vector<long> * >::iterator itervi;
        for( itervi = _int1D.begin(); itervi != _int1D.end(); itervi++ )
        {
                CommandPtr tempCommand = CommandPtr( new Command() );
                tempCommand->SetCommandName( itervi->first );
                ves::open::xml::DataValuePairPtr dataDVP( new ves::open::xml::DataValuePair() );
                dataDVP->SetData( itervi->first, *( itervi->second ) );
                tempCommand->AddDataValuePair( dataDVP );
                m_veModel->SetInput( tempCommand );
        }
    }

    {
        ///Set the 1d double data
        std::map<std::string, std::vector<double> * >::iterator itervd;
        for( itervd = _double1D.begin(); itervd != _double1D.end(); itervd++ )
        {
                CommandPtr tempCommand = CommandPtr( new Command() );
                tempCommand->SetCommandName( itervd->first );
                ves::open::xml::DataValuePairPtr dataDVP( new ves::open::xml::DataValuePair() );
                dataDVP->SetData( itervd->first, *( itervd->second ) );
                tempCommand->AddDataValuePair( dataDVP );
                m_veModel->SetInput( tempCommand );
        }
    }

    {
        ///Set the 1d string data
        std::map<std::string, std::vector<std::string>* >::iterator itervs;
        for( itervs = _string1D.begin(); itervs != _string1D.end(); itervs++ )
        {
                CommandPtr tempCommand = CommandPtr( new Command() );
                tempCommand->SetCommandName( itervs->first );
                ves::open::xml::DataValuePairPtr dataDVP( new ves::open::xml::DataValuePair() );
                dataDVP->SetData( itervs->first, *( itervs->second ) );
                tempCommand->AddDataValuePair( dataDVP );
                m_veModel->SetInput( tempCommand );
        }
    }

    {
        ///Set the 2d string data
        std::map<std::string, 
            std::vector< std::vector<std::string> >* >::iterator iterv2s;
        for( iterv2s = _string2D.begin(); iterv2s != _string2D.end(); iterv2s++ )
        {
            std::string temp2d( iterv2s->first );
            std::vector< std::vector< std::string > > temp2v;
            temp2v = *(iterv2s->second);
            CommandPtr tempCommand = CommandPtr( new Command() );
            tempCommand->SetCommandName( iterv2s->first );
            ves::open::xml::DataValuePairPtr dataDVP( 
                new ves::open::xml::DataValuePair() );
            dataDVP->SetData( iterv2s->first, *( iterv2s->second ) );
            tempCommand->AddDataValuePair( dataDVP );
            m_veModel->SetInput( tempCommand );
        }
    }
    // EPRI TAG
    if( financial_dlg != NULL )
    {
        CommandPtr tempCommand = CommandPtr( new Command() );
        tempCommand->SetCommandName( "EPRI TAG" );

        ves::open::xml::DataValuePairPtr dataDVP( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "USE_FINANCIAL", static_cast< long >( financial_dlg->_use_data ) );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC00", financial_dlg->_cc00_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC01", financial_dlg->_cc01_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC02", financial_dlg->_cc02_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC03", financial_dlg->_cc03_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC04", financial_dlg->_cc04_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC05", financial_dlg->_cc05_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC06", financial_dlg->_cc06_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC07", financial_dlg->_cc07_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "CC08", financial_dlg->_cc08_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "OM00", financial_dlg->_om00_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "OM01", financial_dlg->_om01_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "OM02", financial_dlg->_om02_d );
        tempCommand->AddDataValuePair( dataDVP );

        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
        dataDVP->SetData( "OM03", financial_dlg->_om03_d );
        tempCommand->AddDataValuePair( dataDVP );
    }

    return m_veModel;
}
/////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetVEModel( ves::open::xml::model::ModelWeakPtr tempModel )
{
    m_veModel = tempModel.lock();

    //veModel->SetObjectFromXMLData( modelElement );
    SetName( wxString( m_veModel->GetPluginName().c_str(), wxConvUTF8 ) );
    id = m_veModel->GetModelID();
    parentModel = m_veModel->GetParentModel();
    std::string tempFilename = m_veModel->GetIconFilename();
    pos.x = m_veModel->GetIconLocation()->GetPoint().first;
    pos.y = m_veModel->GetIconLocation()->GetPoint().second;

    //unsigned int numInputs = m_veModel->GetNumberOfInputs();
    const std::vector< CommandPtr > inputsVec = m_veModel->GetInputs();

    for( size_t i = 0; i < inputsVec.size(); ++i )
    {
        CommandPtr commandData = inputsVec.at( i );
        // Add if statement for input variables
        //if "EPRI TAG"
        //else
        {
            for( unsigned int k = 0; k < commandData->GetNumberOfDataValuePairs(); ++k )
            {
                DataValuePairPtr tempData = commandData->GetDataValuePair( k );
                std::string dataName = tempData->GetDataName();
                std::string dataType = tempData->GetDataType();
                // to grab the data from the maps properly
                std::map<std::string, long* >::iterator iteri;
                std::map<std::string, double* >::iterator iterd;
                std::map<std::string, std::string* >::iterator iters;
                std::map<std::string, std::vector<long>* >::iterator itervi;
                std::map<std::string, std::vector<double>* >::iterator itervd;
                std::map<std::string, std::vector<std::string>* >::iterator itervs;
                std::map<std::string, 
                    std::vector< std::vector<std::string> >* >::iterator iterv2s;

                if( std::string( "FLOAT" ) == dataType )
                {
                    iterd = _double.find( dataName );
                    if( iterd != _double.end() )
                        tempData->GetData( *( iterd->second ) );
                }
                else if( std::string( "LONG" ) == dataType )
                {
                    iteri = _int.find( dataName );
                    if( iteri != _int.end() )
                    {
                        tempData->GetData( *( iteri->second ) );
                    }
                }
                else if( std::string( "STRING" ) == dataType )
                {
                    iters = _string.find( dataName );
                    if( iters != _string.end() )
                        tempData->GetData( *( iters->second ) );
                }
                else if( std::string( "1DSTRING" ) == dataType )
                {
                    itervs = _string1D.find( dataName );
                    if( itervs != _string1D.end() )
                        tempData->GetData( *( itervs->second ) );
                }
                else if( std::string( "1DDOUBLE" ) == dataType )
                {
                    itervd = _double1D.find( dataName );
                    if( itervd != _double1D.end() )
                        tempData->GetData( *( itervd->second ) );
                }
                else if( std::string( "1DLONG" ) == dataType )
                {
                    itervi = _int1D.find( dataName );
                    if( itervi != _int1D.end() )
                        tempData->GetData( *( itervi->second ) );
                }
                else if( std::string( "2DSTRING" ) == dataType )
                {
                    iterv2s = _string2D.find( dataName );
                    if( iterv2s != _string2D.end() )
                        tempData->GetData( *( iterv2s->second ) );
                }
            
                /*else if(std::string( "XMLOBJECT" ) == dataType )
                {
                   iteri = _double.find( dataName );
                   if(iteri != _double.end() );
                   tempData->GetData( *(iteri->second) );
                   tempData->GetData( *(_int1D[ dataName ]) );
                }*/
            }
        }
        /*
          // EPRI TAG
          long uf = 0;
          if(mod_pack.getVal("USE_FINANCIAL", uf)) {

            if(financial_dlg == NULL)
              financial_dlg = new FinancialDialog (NULL, (wxWindowID)-1);

            financial_dlg->_use_data = uf;

            financial_dlg->_cc00_d = mod_pack.getDouble("CC00");
            financial_dlg->_cc01_d = mod_pack.getDouble("CC01");
            financial_dlg->_cc02_d = mod_pack.getDouble("CC02");
            financial_dlg->_cc03_d = mod_pack.getDouble("CC03");
            financial_dlg->_cc04_d = mod_pack.getDouble("CC04");
            financial_dlg->_cc05_d = mod_pack.getDouble("CC05");
            financial_dlg->_cc06_d = mod_pack.getDouble("CC06");
            financial_dlg->_cc07_d = mod_pack.getDouble("CC07");
            financial_dlg->_cc08_d = mod_pack.getDouble("CC08");

            financial_dlg->_om00_d = mod_pack.getDouble("OM00");
            financial_dlg->_om01_d = mod_pack.getDouble("OM01");
            financial_dlg->_om02_d = mod_pack.getDouble("OM02");
            financial_dlg->_om03_d = mod_pack.getDouble("OM03");
          }
        */
    }

    inputPort.clear();
    outputPort.clear();
    //Setup the ports so that the plugin can access them.
    for( size_t i = 0; i < m_veModel->GetNumberOfPorts(); ++i )
    {
        ves::open::xml::model::PortPtr tempPort = m_veModel->GetPort( i );
        if( tempPort->GetDataFlowDirection() == std::string( "input" ) )
        {
            inputPort.push_back( tempPort );
        }
        else if( tempPort->GetDataFlowDirection() == std::string( "output" ) )
        {
            outputPort.push_back( tempPort );
        }
        else
        {
            wxMessageDialog( m_canvas, _( "Improperly formated ves file." ),
                             _( "VES File Read Error" ), wxOK | wxICON_ERROR, wxDefaultPosition );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RegistVar( std::string vname, long* var )
{
    _int[vname] = var;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RegistVar( std::string vname, double* var )
{
    _double[vname] = var;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RegistVar( std::string vname, std::string* var )
{
    _string[vname] = var;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RegistVar( std::string vname, std::vector<long>* var )
{
    _int1D[vname] = var;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RegistVar( std::string vname, std::vector<double>* var )
{
    _double1D[vname] = var;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RegistVar( std::string vname, std::vector<std::string>* var )
{
    _string1D[vname] = var;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RegistVar( std::string vname, 
                             std::vector< std::vector<std::string> >* var )
{
    _string2D[vname] = var;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::FinancialData()
{
    if( financial_dlg == NULL )
        financial_dlg = new FinancialDialog( m_canvas, wxID_ANY );

    financial_dlg->Show();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::ViewInputVariables( void )
{
    if( inputsDialog )
    {
        inputsDialog->Destroy();
        inputsDialog = 0;
    }

    const std::vector< CommandPtr > inputsVec = m_veModel->GetInputs();

    size_t numInputs = inputsVec.size();
    ///First try check and see if there are any local variables
    if( numInputs == 0 )
    {
        //serviceList->GetMessageLog()->
        //    SetMessage( "Model contains no input variables\n" );
        MessageLog( "Model contains no input variables\n" );
        ///The code below is not robust so...
        return;
        

        ///Query for the inputs
        std::string compName = GetVEModel()->GetPluginName();

        ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
        returnState->SetCommandName( "getInputModuleParamList" );
        ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
        data->SetData( std::string( "ModuleName" ), compName );
        returnState->AddDataValuePair( data );

        std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
        nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

        ves::open::xml::XMLReaderWriter commandWriter;
        std::string status = "returnString";
        commandWriter.UseStandaloneDOMDocumentManager();
        commandWriter.WriteXMLDocument( nodes, status, "Command" );

        //Get inputs
        std::string nw_str = serviceList->Query( status );
        if( nw_str.empty() || !nw_str.compare( "NULL") )
        {
            return;
        }
        //wxString title( compName.c_str(),wxConvUTF8);
        //TextResultDialog * results = new TextResultDialog(this, title);
        //QueryInputsDlg * results = new QueryInputsDlg(this);
        //ParamsDlg* params = new ParamsDlg(networkFrame);
        //params->SetPosition( wxPoint(dialogSize.x, dialogSize.y) );
        ves::open::xml::XMLReaderWriter networkReader;
        networkReader.UseStandaloneDOMDocumentManager();
        networkReader.ReadFromString();
        //serviceList->GetMessageLog()->SetMessage(nw_str.c_str());
        networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
        std::vector< ves::open::xml::XMLObjectPtr > objectVector = 
            networkReader.GetLoadedXMLObjects();
        //std::ostringstream output;
        //output << objectVector.size()<<std::endl;
        //serviceList->GetMessageLog()->SetMessage(output.str().c_str());
        if( objectVector.size() > 0 )
        {
            ves::open::xml::CommandPtr cmd = 
                boost::dynamic_pointer_cast<Command>( objectVector.at( 0 ) );
            ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );
        }
        /*std::vector< std::string > temp_vector;
        pair->GetData(temp_vector);

        params->SetCompName(compName.c_str());
        params->SetServiceList(serviceList);
        params->SetDialogType("input");
        for (int i=0; i < temp_vector.size(); i++) 
            params->AppendList(temp_vector[i].c_str());
        params->ShowModal();*/
    }

    inputsDialog = new SummaryResultDialog( GetPluginParent(), 
        wxT( "Input Variables" ), wxSize( 560, 400 ), inputsVec );
    ConfigurePluginDialogs( inputsDialog );

    // Get all the results form the model
    inputsDialog->Show();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::ViewResultsVariables( void )
{

    if( resultsDialog )
    {
        resultsDialog->Destroy();
        resultsDialog = 0;
    }

    std::string unitResultsData = "NULL";
    CommandPtr resultsCommand( new Command() );
    resultsCommand->SetCommandName( "Get XML Model Results" );
    //Get model id and unit name
    //gets tagged as vendorUnit (model name) and modelId (number id not uuid)
    DataValuePairPtr vendorData( new DataValuePair() );
    vendorData->SetData( "vendorUnit", m_veModel->GetPluginName() );
    resultsCommand->AddDataValuePair( vendorData );
    DataValuePairPtr moduleIdData( new DataValuePair() );
    moduleIdData->SetData( "moduleId", 
        static_cast< unsigned int >( m_veModel->GetModelID() ) );
    resultsCommand->AddDataValuePair( moduleIdData );

    //Then parse command
    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    std::string resultsData( "returnString" );
    nodes.push_back( std::pair< CommandPtr, std::string  >(
        resultsCommand, std::string( "vecommand" ) ) );

    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.WriteXMLDocument( nodes, resultsData, "Command" );
    //Now query the unit for data
    unitResultsData = serviceList->Query( resultsData.c_str() );
    
    std::vector< CommandPtr > resultsVec;

    if( unitResultsData != "NULL" )
    {
        XMLReaderWriter networkReader;
        networkReader.UseStandaloneDOMDocumentManager();
        networkReader.ReadFromString();
        networkReader.ReadXMLData( unitResultsData, "Command", "vecommand" );
        std::vector< XMLObjectPtr > objectVector = 
            networkReader.GetLoadedXMLObjects();
        CommandPtr tempResult =  
            boost::dynamic_pointer_cast<ves::open::xml::Command>( 
            objectVector.at( 0 ) );
        if( tempResult )
        {
            m_veModel->SetResult( tempResult );
            resultsVec.push_back( tempResult );
        }
    }

    if( resultsVec.size() == 0 )
    {
        //serviceList->GetMessageLog()->SetMessage( 
        //    "Model contains no results variables\n" );
        MessageLog( "Model contains no results variables\n" );
        return;
    }

    resultsDialog = new SummaryResultDialog( GetPluginParent(), 
        wxT( "Results Variables" ), wxSize( 560, 400 ), resultsVec );

    resultsDialog->Show();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetPluginNameDialog( void )
{
    wxTextEntryDialog newPluginName( 0,
                                     _( "Enter the name for your UI plugin:" ),
                                     _( "Set UI Plugin Name..." ),
                                     mPluginName, wxOK | wxCANCEL );

    if( newPluginName.ShowModal() == wxID_OK )
    {
        SetName( newPluginName.GetValue() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetCORBAService( ves::conductor::util::CORBAServiceList* serviceList )
{
    this->serviceList = serviceList;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetXMLDataBufferEngine( ves::conductor::XMLDataBufferEngine* bufferEngine )
{
    mDataBufferEngine = bufferEngine;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetUserPreferencesDataBuffer( ves::conductor::UserPreferencesDataBuffer* prefBuffer )
{
    mUserPrefBuffer = prefBuffer;
}
/////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetImageIcon( std::string path, float rotation, int mirror, float scale )
{
    //Try and find default icons if needed
    std::map< std::string, wxImage >::iterator iter = defaultIconMap.find( path );
    if( iter != defaultIconMap.end() )
    {
        iconFilename = path;
        //now scale it up or down according to the specified scale
        mIconW = iter->second.GetWidth();
        mIconH = iter->second.GetHeight();

        delete mMyIcon;
        mMyIcon = new wxBitmap( iter->second );

        n_pts = 4;

        poly[0] = wxPoint( 0, 0 );
        poly[1] = wxPoint( mIconW - 1, 0 );
        poly[2] = wxPoint( mIconW - 1, mIconH - 1 );
        poly[3] = wxPoint( 0, mIconH - 1 );
        return;
    }

    wxImage image;
    //Try to find the default aspen icons
    std::string fullPath = path;
    std::map< std::string, char** > aspenPlusIconMap = GetAspenPlusIconMap();
    std::map< std::string, char** >::iterator aspenIconIter;
    aspenIconIter = aspenPlusIconMap.find( fullPath );
    if( aspenIconIter != aspenPlusIconMap.end() )
    {
        wxImage xpmImage( aspenIconIter->second );
        image = xpmImage;
    }
    else
    {
        //Now see if the user has any jpgs in
        //the 2dicons directory for the application
        std::ifstream exists( fullPath.c_str() );
        if( exists.fail() )
        {
            return;
        }
        image.LoadFile( wxString( fullPath.c_str(), wxConvUTF8 ), wxBITMAP_TYPE_ANY );
    }
    iconFilename = path;

    if( mirror == 1 )
    {
        image = image.Mirror( true );
    }
    else if( mirror == 2 )
    {
        image = image.Mirror( false );
    }
    else if( mirror == 3 )
    {
        image = image.Mirror( true );
        image = image.Mirror( false );
    }

    double PI = 3.14159265;
    image = image.Rotate(( rotation * PI ) / 180, wxPoint( 0, 0 ) );

    mIconW = image.GetWidth() - 1;
    mIconH = image.GetHeight() - 1;

    //now scale it up or down according to the specified scale
    mIconW = static_cast< int >( mIconW * scale );
    mIconH = static_cast< int >( mIconH * scale );

    delete mMyIcon;
    mMyIcon = new wxBitmap( image.Scale( mIconW, mIconH ) );
    //mMyIcon=new wxBitmap(image);

    n_pts = 4;

    poly[0] = wxPoint( 0, 0 );
    poly[1] = wxPoint( mIconW - 1, 0 );
    poly[2] = wxPoint( mIconW - 1, mIconH - 1 );
    poly[3] = wxPoint( 0, mIconH - 1 );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetImage( wxImage& image )
{    
    mIconW = static_cast< int >( image.GetWidth() );
    mIconH = static_cast< int >( image.GetHeight() );

    delete mMyIcon;
    mMyIcon = new wxBitmap( image );
    poly[0] = wxPoint( 0, 0 );
    poly[1] = wxPoint( mIconW - 1, 0 );
    poly[2] = wxPoint( mIconH - 1, mIconH - 1 );
    poly[3] = wxPoint( 0, mIconH - 1 );
    return;
}
////////////////////////////////////////////////////////////////////////////////
wxBitmap* UIPluginBase::GetIconImage( )
{
    return mMyIcon;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnDClick( wxMouseEvent &event )
{
    // This function opens a plugins dialog when double clicked on the design canvas
    wxClientDC dc( m_canvas );
    m_canvas->DoPrepareDC( dc );
    dc.SetUserScale( userScale->first, userScale->second );
    wxPoint evtpos = event.GetLogicalPosition( dc );
    //If this is not the plugin then move on to the next one
    if( !SelectMod( evtpos.x, evtpos.y ) )
    {
        event.Skip();
        return;
    }
    CreateUserDialog( evtpos );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::CreateUserDialog( wxPoint WXUNUSED( extpos ) )
{
    SetActiveModel();
    
    // now show the custom dialog with no parent for the wxDialog
    UIDialog* hello = UI( GetPluginParent() );
    hello->SetCORBAServiceList( serviceList );
    hello->SetUIPluginBase( this );

    if( hello != NULL )
    {
        hello->Show();
    }

    //necessary for setting the canvas active to handle keyboard input
    m_canvas->SetFocus();
}
////////////////////////////////////////////////////////////////////////////////
bool UIPluginBase::SelectMod( int x, int y )
{
    // This function checks to see which module your mouse is over based
    // on the x and y location of your mouse on the design canvas
    if( GetBBox().Contains( x, y ) )
    {
        return true;
    }
    return false;
}
//////////////////////////////////////////////////////
//WAS CAUSING CRASHES
//////////////////////////
void  UIPluginBase::OnShowResult( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    char* result = 0;

    if( !serviceList->IsConnectedToCE() )
    {
        return;
    }

    try
    {
        //result = exec->GetModuleResult( id );
    }
    catch ( CORBA::Exception & )
    {
        //serviceList->GetMessageLog()->SetMessage( "Maybe Computational Engine is down\n" );
        MessageLog( "Maybe Computational Engine is down\n" );
        return;
    }

    if( std::string( result ) != "" )
    {
        /*Package p;
        p.SetSysId("linkresult.xml");
        p.Load(result, strlen(result));

        // In the new code this would pass in a datavalue pair
        UnPackResult( &p.GetInterfaceVector()[0] );
        UIDialog* hello = Result(NULL);

        if(hello != NULL )
           hello->Show();*/
    }
}
////////////////////////////////////////////////////////////////////////////////
void  UIPluginBase::OnShowUserDialog( wxCommandEvent& WXUNUSED( event ) )
{
    CreateUserDialog( wxPoint(0,0) );
}
////////////////////////////////////////////////////////////////////////////////
void  UIPluginBase::OnShowFinancial( wxCommandEvent& WXUNUSED( event ) )
{
    FinancialData();
}
////////////////////////////////////////////////////////////////////////////////
/*void  UIPluginBase::OnShowAspenName( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    ves::open::xml::model::ModelPtr veModel = GetVEModel();
    wxString title;
    title << wxT( "Aspen Name" );
    wxString desc( veModel->GetModelName().c_str(), wxConvUTF8 );
    wxMessageDialog( m_canvas, desc, title ).ShowModal();
}*/
////////////////////////////////////////////////////////////////////////////////
void  UIPluginBase::OnShowIconChooser( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    IconChooser* m_iconChooser;
    //UIPluginBase* tempPlugin = this;
    //if( m_iconChooser == NULL )
    //{
        m_iconChooser = new IconChooser( GetPluginParent() );
        m_iconChooser->SetPlugin( this );
    //}
    //m_iconChooser->AddIconsDir( wxString( "2dicons", wxConvUTF8 ) );
    //m_iconChooser->SetPlugin( tempPlugin );
    //chooser->SetSize( dialogSize );
    m_iconChooser->Show();
    //delete m_iconChooser;

    //event.SetClientData( this );
    //::wxPostEvent( m_canvas->GetParent(), event );
}
////////////////////////////////////////////////////////////////////////////////
/*void  UIPluginBase::OnQueryInputs( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetModelName();
    compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    while( parentTraverser->GetParentModel() != NULL )
    {
        //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
        compName = "Data.Blocks." + parentTraverser->GetModelName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getInputModuleParamList" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    std::string nw_str = serviceList->Query( status );
    wxString title( compName.c_str(), wxConvUTF8 );
    //TextResultDialog * results = new TextResultDialog(this, title);
    //QueryInputsDlg * results = new QueryInputsDlg(this);
    ParamsDlg* params = new ParamsDlg( m_canvas );
    //params->SetPosition( wxPoint(dialogSize.x, dialogSize.y) );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    //serviceList->GetMessageLog()->SetMessage(nw_str.c_str());
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    //std::ostringstream output;
    //output << objectVector.size()<<std::endl;
    //serviceList->GetMessageLog()->SetMessage(output.str().c_str());
    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<Command>( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );
    std::vector< std::string > temp_vector;
    pair->GetData( temp_vector );

    params->SetCompName( compName.c_str() );
    params->SetServiceList( serviceList );
    params->SetDialogType( "input" );
    for( size_t i = 0; i < temp_vector.size(); i++ )
        params->AppendList( temp_vector[i].c_str() );
    params->ShowModal();
    params->Destroy();
    //serviceList->GetMessageLog()->SetMessage("gather");
    //gather requested inputs
    //std::vector< std::string > temp_vector2;
    //for(int testing = 0; testing < results->GetDataSize(); testing++)
    // temp_vector2.push_back(std::string(results->GetDataString(testing).c_str()));

    //serviceList->GetMessageLog()->SetMessage("submit or not");
    //if it is submit launch request
    //if(results->IsSubmit())
    // this->OnQueryInputModuleProperties(temp_vector2, compName);
}
////////////////////////////////////////////////////////////////////////////////
void  UIPluginBase::OnQueryDynamics( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetModelName();
    //compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    //while( parentTraverser->GetParentModel() != NULL )
    //{
        //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
        //compName = "Data.Blocks." + parentTraverser->GetModelName() + "." + compName;
    //    parentTraverser = parentTraverser->GetParentModel();
    //}

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getModuleParamList" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    std::string nw_str = serviceList->Query( status );
    //std::ofstream packet("packet.txt");
    //packet<<nw_str;
    //packet.close();
    wxString title( compName.c_str(), wxConvUTF8 );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<Command>( objectVector.at( 0 ) );
    AspenDynamicsDialog* params = new AspenDynamicsDialog( m_canvas );
    params->SetComponentName( wxString( compName.c_str(), wxConvUTF8 ) );
    params->SetServiceList( serviceList );
    int numdvps = cmd->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numdvps; i++ )
    {
        ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( i );
        std::vector< std::string > temp_vector;
        pair->GetData( temp_vector );
        params->SetData( wxString( temp_vector[0].c_str(), wxConvUTF8 ), wxString( temp_vector[1].c_str(), wxConvUTF8 ),
            wxString( temp_vector[2].c_str(), wxConvUTF8 ), wxString( temp_vector[3].c_str(), wxConvUTF8 ) );
        //params->SetData( wxString( temp_vector[0].c_str(), wxConvUTF8 ) );
    }
    params->UpdateSizes();
    params->ShowModal();
    params->Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void  UIPluginBase::OnQueryOutputs( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetModelName();
    compName = "Data.Blocks." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    while( parentTraverser->GetParentModel() != NULL )
    {
        //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
        compName = "Data.Blocks." + parentTraverser->GetModelName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getOutputModuleParamList" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    std::string nw_str = serviceList->Query( status );
    wxString title( compName.c_str(), wxConvUTF8 );
    //QueryInputsDlg * results = new QueryInputsDlg(this);
    ParamsDlg * params = new ParamsDlg( m_canvas );
    //params->SetPosition( wxPoint(dialogSize.x, dialogSize.y) );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<Command>( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );
    std::vector< std::string > temp_vector;
    pair->GetData( temp_vector );

    params->SetCompName( compName.c_str() );
    params->SetServiceList( serviceList );
    params->SetDialogType( "output" );
    for( size_t i = 0; i < temp_vector.size(); i++ )
        params->AppendList( temp_vector[i].c_str() );
    params->ShowModal();
    params->Destroy();
    //std::vector< std::string > temp_vector2;
    //for(int testing = 0; testing < results->GetDataSize(); testing++)
    // temp_vector2.push_back(std::string(results->GetDataString(testing).c_str()));

    //if(results->IsSubmit())
    // this->OnQueryOutputModuleProperties(temp_vector2, compName);
}
////////////////////////////////////////////////////////////////////////////////
void  UIPluginBase::OnReinitBlocks( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    std::string compName = GetVEModel()->GetModelName();

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    while( parentTraverser->GetParentModel() != NULL )
    {
        //compName = parentTraverser->GetModelName() +".Data.Blocks." + compName;
        compName = parentTraverser->GetModelName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "reinitBlock" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    serviceList->Query( status );
}*/
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnShowDesc( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    wxString title;
    title << wxT( "Description" );
    wxMessageDialog( m_canvas, mDescription, title ).ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnParaView( wxCommandEvent& WXUNUSED( event ) )
{
    //wxArrayString output;
    // ::wxExecute("paraview", wxEXEC_ASYNC|wxEXEC_MAKE_GROUP_LEADER);
    //::wxShell("paraview");
#ifndef WIN32
    paraThread* para_t = new paraThread( NULL );
    para_t->Create();
    para_t->Run();
#else
    ::wxExecute( "paraview", wxEXEC_ASYNC );
#endif

}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnInputsWindow( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    // Here we launch a dialog for a specific plugins input values
    ViewInputVariables();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnResultsWindow( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    // Here we launch a dialog for a specific plugins input values
    ViewResultsVariables();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnNavigateTo( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Set the active model so that we do not have to in every function
    if( !SetActiveModel() )
    {
        return;
    }

    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "Move to cad" ) );
    ves::open::xml::DataValuePairPtr dataValuePair( 
        new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "NAVIGATE_TO", m_veModel->GetID() );
    veCommand->AddDataValuePair( dataValuePair );
    
    if( event.GetId() == UIPLUGINBASE_NAVTO_SELECT )
    {
        ves::open::xml::DataValuePairPtr selectDVP( 
            new ves::open::xml::DataValuePair() );
        selectDVP->SetData( "Select", "Glow" );
        veCommand->AddDataValuePair( selectDVP );
    }

    bool connected = serviceList->SendCommandStringToXplorer( veCommand );
    boost::ignore_unused_variable_warning( connected );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnOptimizeCAD( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Set the active model so that we do not have to in every function
    if( !SetActiveModel() )
    {
        return;
    }
    
    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "Optimize CAD" ) );
    ves::open::xml::DataValuePairPtr dataValuePair( 
                                                   new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "Optimize CAD", m_veModel->GetID() );
    veCommand->AddDataValuePair( dataValuePair );

    bool connected = serviceList->SendCommandStringToXplorer( veCommand );
    boost::ignore_unused_variable_warning( connected );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnGeometry( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Set the active model so that we do not have to in every function
    if( !SetActiveModel() )
    {
        return;
    }

    // Here we launch a dialog for a specific plugins input values

    if( !cadDialog )
    {
        ves::open::xml::model::ModelPtr veModel = GetVEModel();
        cadDialog = new ves::conductor::util::CADNodeManagerDlg( veModel->AddGeometry(),
                                                                 GetPluginParent(), ::wxNewId() );

        cadDialog->SetSize( dialogSize.x, dialogSize.y, dialogSize.width,
                        dialogSize.height, wxSIZE_AUTO );

        ConfigurePluginDialogs( cadDialog );
    }
    //cadDialog->SetRootCADNode( veModel->GetGeometry() );
    //A modal dialog should no longer be needed since we are using smart ptrs
    cadDialog->Show();
    // Get cadnode back
    /*if( cadDialog->GetRootCADNode() )
    {
        veModel->AddGeometry( cadDialog->GetRootCADNode() );
    }*/
    //cadDialog->Destroy();
    //cadDialog = 0;
}

///////////////////////////////////////////
void UIPluginBase::OnDataSet( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Set the active model so that we do not have to in every function
    if( !SetActiveModel() )
    {
        return;
    }

    // Here we launch a dialog for a specific plugins input values
    if( !m_dataSetLoaderDlg )
    {
        ves::open::xml::model::ModelPtr veModel = GetVEModel();
        m_dataSetLoaderDlg = new ves::conductor::util::DataSetLoaderUI(
                                 GetPluginParent(), ::wxNewId(), SYMBOL_DATASETLOADERUI_TITLE,
                                 SYMBOL_DATASETLOADERUI_POSITION, SYMBOL_DATASETLOADERUI_SIZE,
                                 SYMBOL_DATASETLOADERUI_STYLE, veModel );
        m_dataSetLoaderDlg->SetSize( dialogSize );
        m_dataSetLoaderDlg->SetSize( dialogSize.x, dialogSize.y, dialogSize.width,
                        wxDefaultCoord, wxSIZE_AUTO_HEIGHT );
        ConfigurePluginDialogs( m_dataSetLoaderDlg );
    }

    m_dataSetLoaderDlg->Show();
}
//////////////////////////////////////////////////////////
void UIPluginBase::OnVisualization( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Set the active model so that we do not have to in every function
    if( !SetActiveModel() )
    {
        return;
    }

    //Get the active model ID from the xml data
    ves::open::xml::model::ModelPtr activeXMLModel = GetVEModel();
    const std::string modelID = activeXMLModel->GetID();

    //Get the active model from the CORBA side
    ///Should this be a member variable?
    VjObs::Model* activeCORBAModel;

    try
    {
        activeCORBAModel = serviceList->GetXplorerPointer()->GetModel( modelID.c_str() );
    }
    catch ( CORBA::Exception& )
    {
        //serviceList->GetMessageLog()->SetMessage( "Couldn't find model\n" );//<< modelID<<std::endl;
        MessageLog( "Couldn't find model\n" );//<< modelID<<std::endl;
        return;
    }

    if( activeCORBAModel->dataVector.length() == 0 )
    {
        //serviceList->GetMessageLog()->SetMessage( "Model contains no datasets\n" );//<< modelID<<std::endl;
        MessageLog( "Model contains no datasets\n" );//<< modelID<<std::endl;
        return;
    }

    if( !vistab )
    {
        vistab = new Vistab( activeCORBAModel, GetPluginParent(),
                             SYMBOL_VISTAB_IDNAME,
                             wxString( activeXMLModel->GetPluginName().c_str(), wxConvUTF8 ),
                             SYMBOL_VISTAB_POSITION,
                             SYMBOL_VISTAB_SIZE,
                             SYMBOL_VISTAB_STYLE );
        //vistab->SetSize( dialogSize );
        vistab->SetSize( dialogSize.x, dialogSize.y, dialogSize.width,
                         wxDefaultCoord, wxSIZE_AUTO_HEIGHT );
        ConfigurePluginDialogs( vistab );
    }
    else
    {
        vistab->SetActiveModel( activeCORBAModel );
        vistab->SetSize( dialogSize.x, dialogSize.y, dialogSize.width,
                         wxDefaultCoord, wxSIZE_AUTO_HEIGHT );
    }

    size_t nInformationPackets = activeXMLModel->GetNumberOfInformationPackets();
    if( nInformationPackets == 0 )
    {
        return;
    }

    wxArrayString scalarTextureDatasets;
    wxArrayString vectorTextureDatasets;
    bool hasScalarTextures = false;
    bool hasVectorTextures = false;
    bool isDataSet = false;

    for( size_t i = 0; i < nInformationPackets; i++ )
    {
        ves::open::xml::ParameterBlockPtr paramBlock = activeXMLModel->GetInformationPacket( i );
        size_t numProperties = paramBlock->GetNumberOfProperties();

        for( size_t i = 0; i < numProperties; ++i )
        {
            ves::open::xml::DataValuePairPtr dataValuePair = paramBlock->GetProperty( i );
            if( dataValuePair->GetDataName() == "VTK_TEXTURE_DIR_PATH" )
            {

                size_t textureDataType = dataValuePair->GetDataString().find( "scalars" );
                if( textureDataType < dataValuePair->GetDataString().size() )
                {
                    scalarTextureDatasets.Add( wxString( dataValuePair->GetDataString().c_str(), wxConvUTF8 ) );
                    hasScalarTextures = true;
                }
                else
                {
                    textureDataType = dataValuePair->GetDataString().find( "vectors" );
                    if( textureDataType < dataValuePair->GetDataString().size() )
                    {
                        vectorTextureDatasets.Add( wxString( dataValuePair->GetDataString().c_str(), wxConvUTF8 ) );
                        hasVectorTextures = true;
                    }
                }
            }
            isDataSet = true;
        }
    }

    if( hasScalarTextures )
    {
        vistab->SetTextureData( scalarTextureDatasets, "TEXTURE_SCALARS" );
    }

    if( hasVectorTextures )
    {
        vistab->SetTextureData( vectorTextureDatasets, "TEXTURE_VECTORS" );
    }
    if( !hasScalarTextures && !hasVectorTextures )
    {
        vistab->SetButtonStatus( "TBET", false );
    }


    if( isDataSet )
    {
        int error = vistab->Show();
        boost::ignore_unused_variable_warning( error );
    }
    else
    {
        wxMessageBox( _( "Open a dataset" ), _( "Dataset Failure" ),
                      wxOK | wxICON_INFORMATION );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnSetUIPluginName( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    // Here we launch a dialog for a specific plugins input values
    SetPluginNameDialog();
    GlobalNameUpdate( event );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnZoomSelected( wxCommandEvent& WXUNUSED( event ) )
{
    m_network->GetUserScale()->first = 1;
    m_network->GetUserScale()->second = 1;

    std::pair< int, int > networkSize = m_network->GetNetworkSize( );
    networkSize.first *= m_network->GetUserScale()->first;
    networkSize.second *= m_network->GetUserScale()->second;

    m_canvas->SetUserScale(m_network->GetUserScale()->first, 
        m_network->GetUserScale()->second  );
    m_canvas->SetVirtualSize( networkSize.first, networkSize.second );
    m_canvas->Scroll( pos.x, pos.y );
    m_canvas->Refresh( true );
}

///////////////////////////////////////////////////////////////////////////////
void UIPluginBase::GlobalNameUpdate( wxCommandEvent& event )
{
    //pass event up to hierarchy tree
    event.SetClientData( &id );
    event.SetString( mPluginName );
    ::wxPostEvent( m_canvas, event );

    m_canvas->Refresh( true );
}
//////////////////////////////////////////////////
void UIPluginBase::OnModelSounds( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Set the active model so that we do not have to in every function
    if( !SetActiveModel() )
    {
        return;
    }

    if( !_soundsDlg )
    {
        _soundsDlg = new SoundsPane( GetPluginParent(), GetVEModel() );
        //_soundsDlg->SetSize( dialogSize );
        _soundsDlg->SetSize( dialogSize.x, dialogSize.y, dialogSize.width,
                        wxDefaultCoord, wxSIZE_AUTO_HEIGHT );

        ConfigurePluginDialogs( _soundsDlg );
    }
    _soundsDlg->SetActiveModel( GetVEModel() );
    _soundsDlg->Show();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnMRightDown( wxMouseEvent& event )
{
    // This function opens a plugins dialog when
    // double clicked on the design canvas
    wxClientDC dc( m_canvas );
    m_canvas->DoPrepareDC( dc );
    dc.SetUserScale( userScale->first, userScale->second );
    wxPoint evtpos = event.GetLogicalPosition( dc );
    //If this is not the plugin then move on to the next one
    if( !SelectMod( evtpos.x, evtpos.y ) )
    {
        event.Skip();
        return;
    }

    SendActiveId();
    actionPoint = evtpos;
    highlightFlag = true;
    m_network->UnSelectMod();
    m_network->SetSelectedModule( id );
    //m_canvas->Refresh( true );
    m_canvas->PopupMenu( GetPopupMenu() );

    //necessary for setting the canvas active to handle keyboard input
    //m_canvas->SetFocus();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnSetActiveXplorerModel( wxCommandEvent& WXUNUSED( event ) )
{
    SetActiveModel();
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnSetActivePluginID( wxUpdateUIEvent& event )
{
    int* activeIdTemp = static_cast< int* >( event.GetClientData() );
    //std::cout << *activeIdTemp << std::endl;
    activeId = *activeIdTemp;
    event.Skip();
}
////////////////////////////////////////////////////////////////////////////////
bool UIPluginBase::CheckID()
{
    if( activeId == id )
    {
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool UIPluginBase::SetActiveModel()
{
    // Create the command and data value pairs
    ves::open::xml::DataValuePairPtr dataValuePair(
        new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "CHANGE_ACTIVE_MODEL", m_veModel->GetID() );

    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "CHANGE_ACTIVE_MODEL" ) );
    veCommand->AddDataValuePair( dataValuePair );

    bool connected = serviceList->SendCommandStringToXplorer( veCommand );

    //Clean up memory
    return connected;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetDialogSize( wxRect dialogSize )
{
    this->dialogSize = dialogSize;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnDelMod( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )

    int answer = wxMessageBox( _( "Do you really want to delete this module?" ), 
        _( "Confirmation" ), wxYES_NO );
    if( answer != wxYES )
        return;

    //Now delete the plugin from the module and then remove from the map
    ///This is so that we find the right eventhandler to pop rather than
    ///popping the last one
    //networkFrame->RemoveEventHandler( this );

    ///Now send the erased module to xplorer to delete it as well
    ves::open::xml::DataValuePairPtr dataValuePair( 
        new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "Object ID", m_veModel->GetID() );
    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "DELETE_OBJECT_FROM_NETWORK" ) );
    veCommand->AddDataValuePair( dataValuePair );
    bool connected = serviceList->SendCommandStringToXplorer( veCommand );
    boost::ignore_unused_variable_warning( connected );
    //Clean up memory
    mDataBufferEngine->RemoveModelFromSystem( m_veModel );
    RemovePluginDialogsFromCanvas();
    
    event.SetClientData( &id );
    ::wxPostEvent( m_network, event );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnMakeIntoHierarchy( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )

    int answer = wxMessageBox( _( "Do you really want to make into a hierarchy?" ), 
        _( "Confirmation" ), wxYES_NO );
    if( answer != wxYES )
        return;

    //add xml system
    ves::open::xml::model::SystemPtr system(
        new ves::open::xml::model::System() );
    ves::open::xml::model::NetworkPtr tempXMLNetwork(
        new ves::open::xml::model::Network() );
    system->AddNetwork( tempXMLNetwork );
    GetVEModel()->SetSubSystem( system );

    //add system to system list
    mDataBufferEngine->AddSubSystem( system );
    
    //disables menu entry for make hierarchy
    SetAsHierarchy( );

    //pass info to canvas to draw the new network and model
    event.SetString( wxString( GetVEModel()->GetSubSystem()->GetID().c_str(), wxConvUTF8 ) );
    event.SetClientData( &id );
    ::wxPostEvent( m_canvas, event );

}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetAsHierarchy( )
{
    if( mPopMenu )
    {
        mPopMenu->Enable( UIPLUGINBASE_MAKE_HIER, false );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetDCScale( std::pair< double, double >* scale )
{
    userScale = scale;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetHighlightFlag( bool flag )
{
    highlightFlag = flag;
}
////////////////////////////////////////////////////////////////////////////////
bool UIPluginBase::GetHighlightFlag()
{
    return highlightFlag;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::DrawPlugin( wxDC* dc )
{
    //if hidden
    if(nameFlag)
    {
        DrawIcon( dc );
        DrawID( dc );
        DrawName( dc );
    }

    //if highlighted
    if( highlightFlag )
    {
        if(nameFlag)
        {
            HighlightSelectedIcon( dc );
        }
        DrawPorts( true, dc );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::DrawPorts( bool flag, wxDC* dc )
{
    boost::ignore_unused_variable_warning( flag );
    // flag sets whether we we are erasing the ports or not
    // This function draws the input and output ports on a selected module
    // that is on the design canvas
    wxPoint bport[4];
    bport[0] = wxPoint( 0, 0 );
    bport[1] = wxPoint( 10, 0 );
    bport[2] = wxPoint( 10, 10 );
    bport[3] = wxPoint( 0, 10 );

    wxRect bbox = GetBBox();

    wxBrush old_brush = dc->GetBrush();
    wxPen old_pen = dc->GetPen();

    dc->SetBrush( *wxRED_BRUSH );
    dc->SetPen( *wxBLACK_PEN );
    dc->SetTextForeground( *wxBLACK );

    wxString text;
    int w = 0;
    int h = 0;

    wxCoord xoff, yoff;
    //setup the input ports
    for( size_t i = 0; i < inputPort.size(); i++ )
    {
        wxPoint tempPoint( inputPort[i]->GetPortLocation()->GetPoint().first, inputPort[i]->GetPortLocation()->GetPoint().second );
        // I believe this means move the points in from the edge of the icon
        // by 3 pixles
        // bbox.x returns the global x location and the ports.x returns the x location with respect to bbox.x
        // the same is also true for the y values
        xoff = tempPoint.x + pos.x - 3;
        yoff = tempPoint.y + pos.y - 3;

        // draw the polygon
        dc->DrawPolygon( 4, bport, xoff, yoff );

        //also, need to draw port type
        text = wxString( inputPort[i]->GetPortType().c_str(), wxConvUTF8 );
        dc->GetTextExtent( text, &w, &h );
        dc->DrawText( text, xoff - w - 2, yoff );
    }
        
    dc->SetBrush( *wxCYAN_BRUSH );

    // do the same thing as we did for the output ports
    for( size_t i = 0; i < outputPort.size(); i++ )
    {
        wxPoint tempPoint( outputPort[i]->GetPortLocation()->GetPoint().first, outputPort[i]->GetPortLocation()->GetPoint().second );
        xoff = tempPoint.x + pos.x - 3;
        yoff = tempPoint.y + pos.y - 3;
        dc->DrawPolygon( 4, bport, xoff, yoff );
        //also, need to draw port type
        text = wxString( outputPort[i]->GetPortType().c_str(), wxConvUTF8 );
        dc->GetTextExtent( text, &w, &h );
        dc->DrawText( text, xoff + 12, yoff );
    }

    // restore the default brush and pen settings as stored initially
    dc->SetBrush( old_brush );
    dc->SetPen( old_pen );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::HighlightSelectedIcon( wxDC* dc )
{
    wxPoint bport[5];
    wxPoint tempPoint  = pos;
    //minus 10 because the icon size seems to be smaller than the bbox size
    int tempHeight = GetBBox().GetHeight();
    int tempWidth = GetBBox().GetWidth();
    int highlightBoxWidth = tempWidth;// + 10;
    int highlightBoxHeight = tempHeight;// + 10;

    bport[0] = wxPoint( tempPoint.x, tempPoint.y );
    bport[1] = wxPoint( tempPoint.x + highlightBoxWidth, tempPoint.y );
    bport[2] = wxPoint( tempPoint.x + highlightBoxWidth, tempPoint.y + highlightBoxHeight );
    bport[3] = wxPoint( tempPoint.x, tempPoint.y + highlightBoxHeight );
    bport[4] = wxPoint( tempPoint.x, tempPoint.y );
    ///Draw the highlight
    wxPen old_pen = dc->GetPen();
    dc->SetPen( *wxRED_PEN );
    dc->DrawLines( 5, bport );
    dc->SetPen( old_pen );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::AddPort( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //Account for user scale
    actionPoint.x = actionPoint.x / userScale->first;
    actionPoint.y = actionPoint.y / userScale->second;
    //Convert to local space
    actionPoint.x = actionPoint.x - pos.x;
    actionPoint.y = actionPoint.y - pos.y;
    //Now lets add the port
    AddPortToModel( actionPoint, event.GetId() );

    m_canvas->Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::AddPortToModel( wxPoint& tempPoint, unsigned int typePort )
{
    //get location
    ves::open::xml::model::PointPtr tempLoc( new ves::open::xml::model::Point() );
    std::pair< unsigned int, unsigned int > newPoint;
    newPoint.first = static_cast< unsigned int >( tempPoint.x );
    newPoint.second = static_cast< unsigned int >( tempPoint.y );
    tempLoc->SetPoint( newPoint );
    //Ask what type of port
    ves::open::xml::model::PortPtr port = m_veModel->GetPort( -1 );
    port->SetPortLocation( tempLoc );
    //either input or output
    //port->SetPluginName( ConvertUnicode( mPluginName.c_str() ) );
    //add the port to the model
    //add the port to the internal plugin structure
    if( typePort == UIPLUGINBASE_ADD_INPUT_PORT )
    {
        port->SetDataFlowDirection( "input" );
        inputPort.push_back( port );
    }
    else if( typePort == UIPLUGINBASE_ADD_OUTPUT_PORT )
    {
        port->SetDataFlowDirection( "output" );
        outputPort.push_back( port );
    }
    port->SetPortNumber( outputPort.size() + inputPort.size() );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::DeletePort( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    //get location
    wxPoint temp;
    temp.x =
        static_cast< unsigned int >( actionPoint.x / userScale->first - pos.x );
    temp.y =
        static_cast< unsigned int >( actionPoint.y / userScale->second - pos.y );
    //find port in model
    int acutallDestPortNumber = -1;
    ves::open::xml::model::PortPtr tempPort;
    for( size_t i = 0; i < m_veModel->GetNumberOfPorts(); ++i )
    {
        ves::open::xml::model::PointPtr tempLoc =
            m_veModel->GetPort( i )->GetPortLocation();
        wxPoint tempPoint( tempLoc->GetPoint().first,
                           tempLoc->GetPoint().second );
        if( computenorm( temp, tempPoint ) <= 10 )
        {
            acutallDestPortNumber = m_veModel->GetPort( i )->GetPortNumber();
            tempPort = m_veModel->GetPort( i );
            //delete the port from the model
            std::vector< PortPtr >::iterator iter;
            iter = std::find( inputPort.begin(),
                              inputPort.end(), m_veModel->GetPort( i ) );
            if( iter != inputPort.end() )
            {
                inputPort.erase( iter );
                m_veModel->RemovePort( m_veModel->GetPort( i ) );
                break;
            }

            iter = std::find( outputPort.begin(), outputPort.end(),
                              m_veModel->GetPort( i ) );
            if( iter != outputPort.end() )
            {
                outputPort.erase( iter );
                m_veModel->RemovePort( m_veModel->GetPort( i ) );
                break;
            }
        }
    }
    //delete associated links
    if( tempPort )
    {
        event.SetClientData( &(*tempPort) );
        ::wxPostEvent( m_network, event );
    }
    m_canvas->Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
double UIPluginBase::computenorm( wxPoint pt1, wxPoint pt2 )
{
    return sqrt( double(( pt1.x - pt2.x )*( pt1.x - pt2.x ) + ( pt1.y - pt2.y )*( pt1.y - pt2.y ) ) );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::OnChildDestroy(wxWindowDestroyEvent& event) 
{ 
    wxWindow* w = event.GetWindow(); 
    //std::cout << ConvertUnicode( event.GetEventObject()->GetClassInfo()->GetClassName() ) << std::endl;
    //wxLogMessage( _("destroyed") );
    //std::cout << "destroyed " << std::endl;
    ///erase the found window
    std::map< int, bool >::iterator iter;
    iter = mDialogMemoryMap.find( w->GetId() );
    //std::cout << "****** " << mDialogMemoryMap.size() << std::endl;
    //std::cout << "deleting ui plugin base " << std::endl;
    if( iter != mDialogMemoryMap.end() )
    {
        mDialogMemoryMap.erase( iter );
    }
    else
    {
        //std::cerr << "Problem deleting UIPluginBase dialogs" << std::endl;
    }

    if( !mDialogMemoryMap.empty() )
    {
        return;
    }
    
    pluginDialogPair = 
        std::pair< unsigned int, size_t >( id, mDialogMemoryMap.size() );
    pluginDeleteEvent.SetClientData( &pluginDialogPair );
    m_network->AddPendingEvent( pluginDeleteEvent );    
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RemovePluginDialogsFromCanvas() 
{
    RemoveWindowFromCanvas( dlg );
    RemoveWindowFromCanvas( result_dlg );
    RemoveWindowFromCanvas( port_dlg );
    RemoveWindowFromCanvas( m_dataSetLoaderDlg );
    RemoveWindowFromCanvas( resultsDialog );
    RemoveWindowFromCanvas( portsDialog );
    RemoveWindowFromCanvas( inputsDialog );
    RemoveWindowFromCanvas( _soundsDlg );
    RemoveWindowFromCanvas( m_iconChooser );
    RemoveWindowFromCanvas( vistab );
    RemoveWindowFromCanvas( cadDialog );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::RemoveWindowFromCanvas( wxWindow* window ) 
{
    if( !window )
    {
        return;
    }
    
    GetPluginParent()->RemoveChild( window );
    //window->DestroyChildren();
    //delete window;
    bool delFlag = window->Destroy();
    boost::ignore_unused_variable_warning( delFlag );
    window = 0;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::ConfigurePluginDialogs( wxWindow* window ) 
{
    if( !window )
    {
        return;
    }

    //wxGTK does not implement the destroy event handler so there is no way
    //for plugins in wxGTK apps to send back info when dialogs are destoryed
#ifndef __WXGTK__
    /*mDialogMemoryMap[ window->GetId() ] = true;
    window->Connect( wxEVT_DESTROY, 
        wxWindowDestroyEventHandler(UIPluginBase::OnChildDestroy), 
        NULL, this );*/
#endif
    window->SetExtraStyle( ~wxWS_EX_BLOCK_EVENTS );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::DisconnectPluginDialogsDestroyEvent( wxWindow* window ) 
{
    if( !window )
    {
        return;
    }
    
    delete window;
    window = 0;
    //window->Disconnect( wxEVT_DESTROY, 
    //    wxWindowDestroyEventHandler(UIPluginBase::OnChildDestroy), NULL, this );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::CheckPluginMapOnExit()
{
    if( mDialogMemoryMap.empty() )
    {
        pluginDialogPair = 
        std::pair< unsigned int, size_t >( id, mDialogMemoryMap.size() );
        pluginDeleteEvent.SetClientData( &pluginDialogPair );
        m_network->AddPendingEvent( pluginDeleteEvent );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetNameFlag( bool flag )
{
    nameFlag = flag;
}
////////////////////////////////////////////////////////////////////////////////
bool UIPluginBase::GetNameFlag()
{
    return nameFlag;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::TogglePlugin( wxCommandEvent& event )
{
    UIPLUGIN_CHECKID( event )
    if( event.GetId() == UIPLUGINBASE_TOGGLE_ALL_ON )
    {
        ves::open::xml::DataValuePairPtr dataValuePair( 
            new ves::open::xml::DataValuePair() );
        dataValuePair->SetData( "VE_XPLORER_PLUGIN_ID", "ALL" );
        ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
        veCommand->SetCommandName( 
            std::string( "Xplorer Toggle Plugin Events" ) );
        veCommand->AddDataValuePair( dataValuePair );
        bool connected = serviceList->SendCommandStringToXplorer( veCommand );
        boost::ignore_unused_variable_warning( connected );
    }
    else if( event.GetId() == UIPLUGINBASE_TOGGLE_PLUGIN_ON )
    {
        ves::open::xml::DataValuePairPtr dataValuePair( 
            new ves::open::xml::DataValuePair() );
        dataValuePair->SetData( "VE_XPLORER_PLUGIN_ID", m_veModel->GetID() );
        ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
        veCommand->SetCommandName( 
            std::string( "Xplorer Toggle Plugin Events" ) );
        veCommand->AddDataValuePair( dataValuePair );
        bool connected = serviceList->SendCommandStringToXplorer( veCommand );
        boost::ignore_unused_variable_warning( connected );
    }
}
////////////////////////////////////////////////////////////////////////////////
wxWindow* UIPluginBase::GetPluginParent()
{
    //currently this returns the parent of the canvas
    //corrected an issue with the canvas scrolling when dialogs were created
    return m_canvas->GetParent();
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* UIPluginBase::GetPopupMenu()
{
    wxMenu* baseMenu = SetupPluginBasePopupMenu();
    wxMenu* completeMenu = GetPluginPopupMenu( baseMenu );
    return completeMenu;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SendActiveId()
{
    //send the active id so that each plugin knows what to do
    wxUpdateUIEvent setActivePluginId( UIPLUGINBASE_SET_ACTIVE_PLUGIN );
    setActivePluginId.SetClientData( &id );
    setActivePluginId.SetId( UIPLUGINBASE_SET_ACTIVE_PLUGIN );
    m_canvas->GetEventHandler()->ProcessEvent( setActivePluginId );
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* UIPluginBase::GetPluginPopupMenu( wxMenu* baseMenu )
{
    return baseMenu;
}
////////////////////////////////////////////////////////////////////////////////
wxMenu* UIPluginBase::SetupPluginBasePopupMenu()
{
    //create the menu
    if( mPopMenu )
    {
        return mPopMenu;
    }
    mPopMenu = new wxMenu();
    wxString menuName = mPluginName + wxString( " Menu", wxConvUTF8 );
    mPopMenu->SetTitle( menuName );
    //mPopMenu->Append( UIPLUGINBASE_USER_DIALOG, _( "User Dialog" ) );
    //mPopMenu->Enable( UIPLUGINBASE_USER_DIALOG, true );
    //mPopMenu->Append( UIPLUGINBASE_SHOW_RESULT, _( "Show Module Result" ) );
    //mPopMenu->Enable( UIPLUGINBASE_SHOW_RESULT, false );
    //mPopMenu->Append( UIPLUGINBASE_PARAVIEW, _( "ParaView 3D Result" ) );
    //mPopMenu->Enable( UIPLUGINBASE_PARAVIEW, false );
    //if( Has3Ddata() )
    //{
    //    mPopMenu->Enable( UIPLUGINBASE_PARAVIEW, true );
    //}
    //mPopMenu->Append( UIPLUGINBASE_SHOW_FINANCIAL, _( "Financial Data" ) );
    //mPopMenu->Enable( UIPLUGINBASE_SHOW_FINANCIAL, true );

    //Conductor Menu
    wxMenu * con_menu = new wxMenu();        
    mPopMenu->Append( UIPLUGINBASE_CONDUCTOR_MENU,   _( "Conductor" ), con_menu,
                     _( "Controls for Conductor" ) );
    mPopMenu->Enable( UIPLUGINBASE_CONDUCTOR_MENU, true );    
    
    //UI for input variables
    con_menu->Append( UIPLUGINBASE_MODEL_INPUTS, _( "Inputs" ) );
    con_menu->Enable( UIPLUGINBASE_MODEL_INPUTS, true );
    //UI for results variables
    con_menu->Append( UIPLUGINBASE_MODEL_RESULTS, _( "Results" ) );
    con_menu->Enable( UIPLUGINBASE_MODEL_RESULTS, true );

    //Icon Menu
    con_menu->Append( UIPLUGINBASE_SHOW_ICON_CHOOSER, _( "Icon" ) );
    con_menu->Enable( UIPLUGINBASE_SHOW_ICON_CHOOSER, true );
    
    //Port Menu
    wxMenu * port_menu = new wxMenu();
    port_menu->Append( UIPLUGINBASE_ADD_INPUT_PORT, _( "Add Input Port" ) );
    port_menu->Enable( UIPLUGINBASE_ADD_INPUT_PORT, true );
    port_menu->Append( UIPLUGINBASE_ADD_OUTPUT_PORT, _( "Add Output Port" ) );
    port_menu->Enable( UIPLUGINBASE_ADD_OUTPUT_PORT, true );
    port_menu->Append( UIPLUGINBASE_DELETE_PORT, _( "Delete Port" ) );
    port_menu->Enable( UIPLUGINBASE_DELETE_PORT, true );
    con_menu->Append( UIPLUGINBASE_PORT_MENU, _( "Ports" ), port_menu,
                     _( "Used to manipulate ports" ) );
    con_menu->Enable( UIPLUGINBASE_PORT_MENU, true );

    //Xplorer Menu
    wxMenu * xplorer_menu = new wxMenu();        
    mPopMenu->Append( UIPLUGINBASE_XPLORER_MENU,   _( "Xplorer" ), xplorer_menu,
                     _( "Controls for Xplorer" ) );
    mPopMenu->Enable( UIPLUGINBASE_XPLORER_MENU, true );    
    
    xplorer_menu->Append( UIPLUGINBASE_NAVTO_SELECT, _( "Navigate To - Select" ) );
    xplorer_menu->Enable( UIPLUGINBASE_NAVTO_SELECT, true );

    xplorer_menu->Append( UIPLUGINBASE_NAVTO, _( "Navigate To" ) );
    xplorer_menu->Enable( UIPLUGINBASE_NAVTO, true );

    xplorer_menu->Append( UIPLUGINBASE_OPTIMIZE_CAD, _( "Optimize CAD" ) );
    xplorer_menu->Enable( UIPLUGINBASE_OPTIMIZE_CAD, true );

    //Make a specific plusing active in xplorer
    xplorer_menu->Append( UIPLUGINBASE_SET_ACTIVE_MODEL, _( "Activate" ) );
    xplorer_menu->Enable( UIPLUGINBASE_SET_ACTIVE_MODEL, true );

    //Toggle Plugin Menu
    wxMenu* pluginMenu = new wxMenu();
    pluginMenu->Append( UIPLUGINBASE_TOGGLE_ALL_ON, _( "Toggle All On" ) );
    pluginMenu->Enable( UIPLUGINBASE_TOGGLE_ALL_ON, true );
    pluginMenu->Append( UIPLUGINBASE_TOGGLE_PLUGIN_ON, _( "Toggle Plugin On" ) );
    pluginMenu->Enable( UIPLUGINBASE_TOGGLE_PLUGIN_ON, true );
    xplorer_menu->Append( UIPLUGINBASE_TOGGLE_MENU, _( "Toggle Plugin" ), pluginMenu,
                     _( "Used to toggle plugin" ) );
    xplorer_menu->Enable( UIPLUGINBASE_TOGGLE_MENU, true );

    //Sounds dialog
    xplorer_menu->Append( UIPLUGINBASE_ACTIVE_MODEL_SOUNDS, _( "Sounds" ) );
    xplorer_menu->Enable( UIPLUGINBASE_ACTIVE_MODEL_SOUNDS, true );

    mPopMenu->AppendSeparator();
     // GUI to configure geometry for graphical env
    mPopMenu->Append( UIPLUGINBASE_GEOMETRY, _( "Geometry Config" ) );
    mPopMenu->Enable( UIPLUGINBASE_GEOMETRY, true );
    // GUI to configure dataset for graphical env
    mPopMenu->Append( UIPLUGINBASE_DATASET, _( "Data Set Config" ) );
    mPopMenu->Enable( UIPLUGINBASE_DATASET, true );
    //UI for vis variables
    mPopMenu->Append( UIPLUGINBASE_VISUALIZATION, _( "Visualization" ) );
    mPopMenu->Enable( UIPLUGINBASE_VISUALIZATION, true );
    mPopMenu->AppendSeparator();

    mPopMenu->Append( UIPLUGINBASE_SHOW_DESC, _( "Description" ) );
    mPopMenu->Enable( UIPLUGINBASE_SHOW_DESC, true );
    //Set the plugin name for a model
    mPopMenu->Append( UIPLUGINBASE_SET_UI_PLUGIN_NAME, _( "Rename" ) );
    mPopMenu->Enable( UIPLUGINBASE_SET_UI_PLUGIN_NAME, true );
    mPopMenu->Append( UIPLUGINBASE_ZOOM, _( "Zoom" ) );
    mPopMenu->Enable( UIPLUGINBASE_ZOOM, true );
    mPopMenu->Append( UIPLUGINBASE_DEL_MOD, _( "Delete" ) );
    mPopMenu->Enable( UIPLUGINBASE_DEL_MOD, true );
    mPopMenu->Append( UIPLUGINBASE_MAKE_HIER, _( "Make Into Hierarchy" ) );
    mPopMenu->Enable( UIPLUGINBASE_MAKE_HIER, true );
    
    //mPopMenu->SetClientData( &id );
    
    return mPopMenu;
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::MessageLog( const char* msg )
{
    wxLogMessage(  wxString( msg, wxConvUTF8 ) );
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetPluginType( const std::string& pluginType )
{
    m_pluginType = pluginType;
}
////////////////////////////////////////////////////////////////////////////////
