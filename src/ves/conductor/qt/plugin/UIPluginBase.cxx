/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/conductor/qt/plugin/UIPluginBase.h>
#include <ves/xplorer/command/CommandManager.h>
#include <ves/xplorer/Model.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Port.h>

#include <iostream>

namespace ves
{
namespace conductor
{

using namespace ves::open::xml;
using namespace ves::open::xml::model;

UIPluginBase::UIPluginBase()
    :
    m_veModel( ModelPtr() ),
    m_xplorerPlugin( 0 )
{
    ;
}

UIPluginBase::~UIPluginBase()
{

}


unsigned int UIPluginBase::GetID()
{
    return m_id;
}

void UIPluginBase::SetPluginType( const std::string& pluginType )
{
    m_pluginType = pluginType;
}


ves::open::xml::model::ModelPtr UIPluginBase::GetVEModel( void )
{
    if( !m_xplorerPlugin )
    {
        return ves::open::xml::model::ModelPtr();
    }

    if( !m_veModel )
    {
        if( !m_xplorerPlugin->GetCFDModel() )
        {
            std::cout <<
                      "|\tThe UI plugin does not have access to the ves::open::xml::model."
                      << std::endl;
            return ves::open::xml::model::ModelPtr();
        }
        m_veModel = m_xplorerPlugin->GetCFDModel()->GetModelData();
    }

    if( m_pluginName.empty() )
    {
        m_pluginName = "PleaseDefineClassName";
    }

    m_veModel->SetPluginName( m_pluginName );
    m_veModel->SetPluginType( m_pluginType );
    m_veModel->SetModelID( m_id );
    //m_veModel->SetIconFilename( iconFilename );
    //m_veModel->GetIconLocation()->SetPoint( std::pair< unsigned int, unsigned int >( pos.x, pos.y ) );

    {
        ///Set the int data
        std::map<std::string, long*>::iterator iteri;
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
        std::map<std::string, double*>::iterator iterd;
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
        std::map<std::string, std::string*>::iterator iters;
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
        std::map < std::string,
            std::vector< std::vector<std::string> >* >::iterator iterv2s;
        for( iterv2s = _string2D.begin(); iterv2s != _string2D.end(); iterv2s++ )
        {
            std::string temp2d( iterv2s->first );
            std::vector< std::vector< std::string > > temp2v;
            temp2v = *( iterv2s->second );
            CommandPtr tempCommand = CommandPtr( new Command() );
            tempCommand->SetCommandName( iterv2s->first );
            ves::open::xml::DataValuePairPtr dataDVP(
                new ves::open::xml::DataValuePair() );
            dataDVP->SetData( iterv2s->first, *( iterv2s->second ) );
            tempCommand->AddDataValuePair( dataDVP );
            m_veModel->SetInput( tempCommand );
        }
    }

    // What's the deal here? None of this code actually does anything that I can
    // tell -- RPT
    // EPRI TAG
    //    if( financial_dlg != NULL )
    //    {
    //        CommandPtr tempCommand = CommandPtr( new Command() );
    //        tempCommand->SetCommandName( "EPRI TAG" );

    //        ves::open::xml::DataValuePairPtr dataDVP( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "USE_FINANCIAL", static_cast< long >( financial_dlg->_use_data ) );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC00", financial_dlg->_cc00_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC01", financial_dlg->_cc01_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC02", financial_dlg->_cc02_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC03", financial_dlg->_cc03_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC04", financial_dlg->_cc04_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC05", financial_dlg->_cc05_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC06", financial_dlg->_cc06_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC07", financial_dlg->_cc07_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "CC08", financial_dlg->_cc08_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "OM00", financial_dlg->_om00_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "OM01", financial_dlg->_om01_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "OM02", financial_dlg->_om02_d );
    //        tempCommand->AddDataValuePair( dataDVP );

    //        dataDVP = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    //        dataDVP->SetData( "OM03", financial_dlg->_om03_d );
    //        tempCommand->AddDataValuePair( dataDVP );
    //    }

    return m_veModel;
}


void UIPluginBase::SetVEModel( ves::open::xml::model::ModelPtr tempModel )
{
    //m_veModel = tempModel.lock();
    m_veModel = tempModel;

    //    SetName( wxString( m_veModel->GetPluginName().c_str(), wxConvUTF8 ) );
    m_id = m_veModel->GetModelID();
    m_parentModel = m_veModel->GetParentModel();
    std::string tempFilename = m_veModel->GetIconFilename();
    //    pos.x = m_veModel->GetIconLocation()->GetPoint().first;
    //    pos.y = m_veModel->GetIconLocation()->GetPoint().second;

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
                std::map < std::string,
                    std::vector< std::vector<std::string> >* >::iterator iterv2s;

                if( std::string( "FLOAT" ) == dataType )
                {
                    iterd = _double.find( dataName );
                    if( iterd != _double.end() )
                    {
                        tempData->GetData( *( iterd->second ) );
                    }
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
                    {
                        tempData->GetData( *( iters->second ) );
                    }
                }
                else if( std::string( "1DSTRING" ) == dataType )
                {
                    itervs = _string1D.find( dataName );
                    if( itervs != _string1D.end() )
                    {
                        tempData->GetData( *( itervs->second ) );
                    }
                }
                else if( std::string( "1DDOUBLE" ) == dataType )
                {
                    itervd = _double1D.find( dataName );
                    if( itervd != _double1D.end() )
                    {
                        tempData->GetData( *( itervd->second ) );
                    }
                }
                else if( std::string( "1DLONG" ) == dataType )
                {
                    itervi = _int1D.find( dataName );
                    if( itervi != _int1D.end() )
                    {
                        tempData->GetData( *( itervi->second ) );
                    }
                }
                else if( std::string( "2DSTRING" ) == dataType )
                {
                    iterv2s = _string2D.find( dataName );
                    if( iterv2s != _string2D.end() )
                    {
                        tempData->GetData( *( iterv2s->second ) );
                    }
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

    m_inputPorts.clear();
    m_outputPorts.clear();
    //Setup the ports so that the plugin can access them.
    for( size_t i = 0; i < m_veModel->GetNumberOfPorts(); ++i )
    {
        ves::open::xml::model::PortPtr tempPort = m_veModel->GetPort( i );
        if( tempPort->GetDataFlowDirection() == std::string( "input" ) )
        {
            m_inputPorts.push_back( tempPort );
        }
        else if( tempPort->GetDataFlowDirection() == std::string( "output" ) )
        {
            m_outputPorts.push_back( tempPort );
        }
        else
        {
            //wxMessageDialog( m_canvas, _( "Improperly formated ves file." ),
            //                 _( "VES File Read Error" ), wxOK | wxICON_ERROR, wxDefaultPosition );
        }
    }
}

void UIPluginBase::SetXplorerPlugin( ves::xplorer::plugin::PluginBase* plugin )
{
    if( !plugin )
    {
        throw "NULL plugin passed to UIPluginBase::SetXplorerPlugin";
    }

    m_xplorerPlugin = plugin;
    //SetVEModel( plugin->GetCFDModel()->GetModelData() );
}

//??? Still needed ???
///allows user to set the image to be displayed on the icon
//void UIPluginBase::SetImageIcon( std::string path, float rotation = 0.0f, int mirror = 0, float scale = 1.0f );

//??? Still needed ???
///allows users creating new plugins and change the icon
//void UIPluginBase::SetImage( QImage& image );

void UIPluginBase::AddPort( unsigned int portType )
{
    if( !( m_veModel.get() ) )
    {
        return;
    }

    ves::open::xml::model::PortPtr port = m_veModel->GetPort( -1 );
    //port->SetPortLocation( tempLoc );

    //add the port to the model
    //add the port to the internal plugin structure
    if( portType == INPUT_PORT )
    {
        port->SetDataFlowDirection( "input" );
        m_inputPorts.push_back( port );
    }
    else if( portType == OUTPUT_PORT )
    {
        port->SetDataFlowDirection( "output" );
        m_outputPorts.push_back( port );
    }
    port->SetPortNumber( m_outputPorts.size() + m_inputPorts.size() );
}

void UIPluginBase::DeletePort( ves::open::xml::model::PortPtr port )
{
    if( !( m_veModel.get() ) )
    {
        return;
    }

    std::vector< PortPtr >::iterator iter;
    iter = std::find( m_inputPorts.begin(),
                      m_inputPorts.end(), port );
    if( iter != m_inputPorts.end() )
    {
        m_inputPorts.erase( iter );
        m_veModel->RemovePort( port );
    }

    iter = std::find( m_outputPorts.begin(), m_outputPorts.end(),
                      port );
    if( iter != m_outputPorts.end() )
    {
        m_outputPorts.erase( iter );
        m_veModel->RemovePort( port );
    }
}

void UIPluginBase::TogglePlugin( unsigned int )
{

    // Unclear why toggling other plugins on would be part of a specific plugin
    //  instance

    //    if( flag == UIPLUGINBASE_TOGGLE_ALL_ON )
    //    {
    //        ves::open::xml::DataValuePairPtr dataValuePair(
    //            new ves::open::xml::DataValuePair() );
    //        dataValuePair->SetData( "VE_XPLORER_PLUGIN_ID", "ALL" );
    //        ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    //        veCommand->SetCommandName(
    //            std::string( "Xplorer Toggle Plugin Events" ) );
    //        veCommand->AddDataValuePair( dataValuePair );

    //        ves::xplorer::command::CommandManager::Instance()->AddXMLCommand( veCommand );
    //    }
    //else if( flag == UIPLUGINBASE_TOGGLE_PLUGIN_ON )
    {
        ves::open::xml::DataValuePairPtr dataValuePair(
            new ves::open::xml::DataValuePair() );
        dataValuePair->SetData( "VE_XPLORER_PLUGIN_ID", m_veModel->GetID() );
        ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
        veCommand->SetCommandName(
            std::string( "Xplorer Toggle Plugin Events" ) );
        veCommand->AddDataValuePair( dataValuePair );

        ves::xplorer::command::CommandManager::instance()->AddXMLCommand( veCommand );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UIPluginBase::SetActiveModel()
{
    ves::open::xml::DataValuePairPtr dataValuePair(
        new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "CHANGE_ACTIVE_MODEL", m_veModel->GetID() );

    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "CHANGE_ACTIVE_MODEL" ) );
    veCommand->AddDataValuePair( dataValuePair );

    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( veCommand );
}
////////////////////////////////////////////////////////////////////////////////

void UIPluginBase::ActivateAssociatedModel( )
{
    //Set the active model so that we do not have to in every function
    SetActiveModel();

    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "Move to cad" ) );
    ves::open::xml::DataValuePairPtr dataValuePair(
        new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "NAVIGATE_TO", m_veModel->GetID() );
    veCommand->AddDataValuePair( dataValuePair );

    //if( event.GetId() == UIPLUGINBASE_NAVTO_SELECT )
    {
        ves::open::xml::DataValuePairPtr selectDVP(
            new ves::open::xml::DataValuePair() );
        selectDVP->SetData( "Select", "Glow" );
        veCommand->AddDataValuePair( selectDVP );
    }

    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( veCommand );
}

void UIPluginBase::OnOptimizeCAD( )
{
    SetActiveModel();

    ves::open::xml::CommandPtr veCommand( new ves::open::xml::Command() );
    veCommand->SetCommandName( std::string( "Optimize CAD" ) );
    ves::open::xml::DataValuePairPtr dataValuePair(
        new ves::open::xml::DataValuePair() );
    dataValuePair->SetData( "Optimize CAD", m_veModel->GetID() );
    veCommand->AddDataValuePair( dataValuePair );

    ves::xplorer::command::CommandManager::instance()->AddXMLCommand( veCommand );
}

//void UIPluginBase::SetNetwork( Network* network )
//{
//    m_network = network;
//}

// ?? Need to understand better what this does
//void UIPluginBase::SendActiveId()
//{
//    //send the active id so that each plugin knows what to do
//    wxUpdateUIEvent setActivePluginId( UIPLUGINBASE_SET_ACTIVE_PLUGIN );
//    setActivePluginId.SetClientData( &id );
//    setActivePluginId.SetId( UIPLUGINBASE_SET_ACTIVE_PLUGIN );
//    m_canvas->GetEventHandler()->ProcessEvent( setActivePluginId );
//}

// This used to simply disable the "Make Hierarchy" entry in the plugin popup
// menu
//void UIPluginBase::SetAsHierarchy()
//{

//}


void UIPluginBase::RegistVar( std::string vname, long* var )
{
    _int[vname] = var;
}

void UIPluginBase::RegistVar( std::string vname, double* var )
{
    _double[vname] = var;
}

void UIPluginBase::RegistVar( std::string vname, std::string* var )
{
    _string[vname] = var;
}

void UIPluginBase::RegistVar( std::string vname, std::vector< long >* var )
{
    _int1D[vname] = var;
}

void UIPluginBase::RegistVar( std::string vname, std::vector< double >* var )
{
    _double1D[vname] = var;
}

void UIPluginBase::RegistVar( std::string vname, std::vector< std::string >* var )
{
    _string1D[vname] = var;
}

void UIPluginBase::RegistVar( std::string vname, std::vector< std::vector<std::string> >* var )
{
    _string2D[vname] = var;
}

int UIPluginBase::GetNumInPorts()
{
    return m_inputPorts.size();
}

PORT UIPluginBase::GetInPorts()
{
    return m_inputPorts;
}

int UIPluginBase::GetNumOutPorts()
{
    return m_outputPorts.size();
}

PORT UIPluginBase::GetOutPorts()
{
    return m_outputPorts;
}

void UIPluginBase::SetNumInputPorts( int num )
{
    if( num < 0 )
    {
        return;
    }

    // Need to check this else we get trapped in while loops below.
    if( !( m_veModel.get() ) )
    {
        return;
    }

    // Add ports if we need more
    while( num > GetNumInPorts() )
    {
        AddPort( INPUT_PORT );
    }

    // Remove ports if we have too many
    while( num < GetNumInPorts() )
    {
        // Remove ports from the end of the ports list
        PORT::iterator iter = GetInPorts().end();
        iter--;
        DeletePort( *iter );
    }
}

void UIPluginBase::SetNumOutputPorts( int num )
{
    if( num < 0 )
    {
        return;
    }

    // Need to check this else we get trapped in while loops below.
    if( !( m_veModel.get() ) )
    {
        return;
    }

    // Add ports if we need more
    while( num > GetNumOutPorts() )
    {
        AddPort( OUTPUT_PORT );
    }

    // Remove ports if we have too many
    while( num < GetNumOutPorts() )
    {
        // Remove ports from the end of the ports list
        PORT::iterator iter = GetOutPorts().end();
        iter--;
        DeletePort( *iter );
    }
}


//    bool UIPluginBase::CheckID();

// This used to close down and delete any dialogs associated with this pluign
//void UIPluginBase::RemovePluginDialogsFromCanvas()
//{

//}


} // namespace conductor
} // namespace ves
