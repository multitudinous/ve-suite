/*************** <auto-copyright.rb BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> **************/
#include <ves/conductor/util/ParamsDlg.h>

#include <wx/msgdlg.h>

using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( ParamsDlg, wxDialog )
    //EVT_CLOSE( ParamsDlg::OnClose )
    EVT_BUTTON( ID_SETBUTTON, ParamsDlg::SetButtonClick )
    EVT_TREE_SEL_CHANGED( ID_PARAMCHOICE, ParamsDlg::ParamChoiceSelected )
END_EVENT_TABLE()

ParamsDlg::ParamsDlg(
    wxWindow *parent, wxWindowID id, const wxString &title,
    const wxPoint &position, const wxSize& size, long style )
        : wxDialog( parent, id, title, position, size, style )
{
    CreateGUIControls();
}
ParamsDlg::~ParamsDlg()
{}

void ParamsDlg::CreateGUIControls()
{
    SetTitle( wxT( "ParamsDialog" ) );
    SetIcon( wxNullIcon );
    SetSize( 8, 8, 775, 400 );
    Center();

    SetButton = new wxButton(
        this, ID_SETBUTTON, wxT( "Set" ), wxPoint( 355, 80 ), wxSize( 40, 25 ),
        0, wxDefaultValidator, wxT( "SetButton" ) );
    SetButton->Enable( false );

    ChildrenLabel = new wxStaticText(
        this, ID_CHILDRENLABEL, wxT( "Children" ), wxPoint( 599, 324 ),
        wxDefaultSize, 0, wxT( "ChildrenLabel" ) );
    HasLabel = new wxStaticText(
        this, ID_HASLABEL, wxT( "Has" ), wxPoint( 599, 312 ),
        wxDefaultSize, 0, wxT( "HasLabel" ) );
    PortTypeLabel = new wxStaticText(
        this, ID_PORTTYPELABEL, wxT( "Port Type" ), wxPoint( 600, 264 ),
        wxDefaultSize, 0, wxT( "PortTypeLabel" ) );
    MultiportLabel = new wxStaticText(
        this, ID_MULTIPORTLABEL, wxT( "Multiport" ), wxPoint( 600, 237 ),
        wxDefaultSize, 0, wxT( "MultiportLabel" ) );
    GenderLabel = new wxStaticText(
        this, ID_GENDERLABEL, wxT( "Gender" ), wxPoint( 600, 212 ),
        wxDefaultSize, 0, wxT( "GenderLabel" ) );
    InOrOutLabel = new wxStaticText(
        this, ID_INOROUTLABEL, wxT( "In or Out" ), wxPoint( 600, 187 ),
        wxDefaultSize, 0, wxT( "InOrOutLabel" ) );
    TypeLabel = new wxStaticText(
        this, ID_TYPELABEL, wxT( "Type" ), wxPoint( 414, 73 ),
        wxDefaultSize, 0, wxT( "TypeLabel" ) );
    DValueLabel = new wxStaticText(
        this, ID_DVALUELABEL, wxT( "Value" ), wxPoint( 418, 247 ),
        wxDefaultSize, 0, wxT( "DValueLabel" ) );
    ListLabel = new wxStaticText(
        this, ID_LISTLABEL, wxT( "List" ), wxPoint( 216, 203 ),
        wxDefaultSize, 0, wxT( "ListLabel" ) );
    MeasureLabel = new wxStaticText( 
        this, ID_MEASURELABEL, wxT( "Measure" ), wxPoint( 216, 148 ),
        wxDefaultSize, 0, wxT( "MeasureLabel" ) );
    QuantityLabel = new wxStaticText(
        this, ID_QUANTITYLABEL, wxT( "Quantity" ), wxPoint( 216, 117 ),
        wxDefaultSize, 0, wxT( "QuantityLabel" ) );
    PromptLabel = new wxStaticText(
        this, ID_PROMPT, wxT( "Prompt" ), wxPoint( 417, 264 ),
        wxDefaultSize, 0, wxT( "PromptLabel" ) );
    DefaultLabel = new wxStaticText(
        this, ID_DEFAULTLABEL, wxT( "Default" ), wxPoint( 418, 233 ),
        wxDefaultSize, 0, wxT( "DefaultLabel" ) );
    LowerLimitLabel = new wxStaticText(
        this, ID_LOWERLIMITLABEL, wxT( "Lower Limit" ), wxPoint( 417, 212 ),
        wxDefaultSize, 0, wxT( "LowerLimitLabel" ) );
    UpperLimitLabel = new wxStaticText(
        this, ID_UPPERLIMITLABEL, wxT( "Upper Limit" ), wxPoint( 417, 185 ),
        wxDefaultSize, 0, wxT( "UpperLimitLabel" ) );
    EnterableLabel = new wxStaticText(
        this, ID_ENTERABLELABEL, wxT( "Enterable" ), wxPoint( 417, 160 ),
        wxDefaultSize, 0, wxT( "EnterableLabel" ) );
    OutputLabel = new wxStaticText(
        this, ID_OUTPUTLABEL, wxT( "Output" ), wxPoint( 417, 134 ),
        wxDefaultSize, 0, wxT( "OutputLabel" ) );
    RecordLabel = new wxStaticText(
        this, ID_RECORDLABEL, wxT( "Record" ), wxPoint( 414, 61 ),
        wxDefaultSize, 0, wxT( "RecordLabel" ) );
    OptionsLabel = new wxStaticText(
        this, ID_OPTIONSLABEL, wxT( "Options" ), wxPoint( 216, 225 ),
        wxDefaultSize, 0, wxT( "OptionsLabel" ) );
    OptionLabel = new wxStaticText(
        this, ID_OPTIONLABEL, wxT( "Option" ), wxPoint( 216, 191 ),
        wxDefaultSize, 0, wxT( "OptionLabel" ) );
    BasisLabel = new wxStaticText( 
        this, ID_BASISLABEL, wxT( "Basis" ), wxPoint( 216, 166 ),
        wxDefaultSize, 0, wxT( "BasisLabel" ) );
    UnitLabel = new wxStaticText(
        this, ID_UNITLABEL, wxT( "Unit of" ), wxPoint( 216, 134 ),
        wxDefaultSize, 0, wxT( "UnitLabel" ) );
    PhysicalLabel = new wxStaticText(
        this, ID_PHYSICALLABEL, wxT( "Physical" ), wxPoint( 216, 104 ),
        wxDefaultSize, 0, wxT( "PhysicalLabel" ) );
    ValueLabel = new wxStaticText(
        this, ID_VALUELABEL, wxT( "Value" ), wxPoint( 216, 86 ),
        wxDefaultSize, 0, wxT( "ValueLabel" ) );
    DimensionLabel = new wxStaticText(
        this, ID_DIMENSIONLABEL, wxT( "Dimension" ), wxPoint( 217, 41 ),
        wxDefaultSize, 0, wxT( "DimensionLabel" ) );
    NodePathLabel = new wxStaticText(
        this, ID_NODEPATHLABEL, wxT( "Path to Node" ), wxPoint( 214, 12 ),
        wxDefaultSize, 0, wxT( "NodePathLabel" ) );
    //ParameterLabel = new wxStaticText(
        //this, ID_PARAMETERLABEL, wxT( "Parameter" ), wxPoint( 305, 10 ),
        //wxDefaultSize, 0, wxT( "ParameterLabel" ) );
    //ParameterLabel->SetFont( wxFont( 8, wxSWISS, wxNORMAL, wxBOLD, FALSE ) );

    WxMemo3 = new wxTextCtrl(
        this, ID_WXMEMO3, wxT( "" ), wxPoint( 599, 83 ), wxSize( 158, 71 ),
        wxTE_READONLY | wxTE_MULTILINE, wxDefaultValidator, wxT( "WxMemo3" ) );
    WxMemo3->SetMaxLength( 0 );
    WxMemo3->SetFocus();
    WxMemo3->SetInsertionPointEnd();
    PromptMemo = new wxTextCtrl(
        this, ID_PROMPTMEMO, wxT( "" ), wxPoint( 474, 261 ), wxSize( 103, 83 ),
        wxTE_READONLY | wxVSCROLL | wxTE_MULTILINE, wxDefaultValidator,
        wxT( "PromptMemo" ) );
    PromptMemo->SetMaxLength( 0 );
    PromptMemo->SetFocus();
    PromptMemo->SetInsertionPointEnd();
    OptionsMemo = new wxTextCtrl(
        this, ID_OPTIONSMEMO, wxT( "" ), wxPoint( 272, 225 ),
        wxSize( 121, 118 ), wxTE_READONLY | wxVSCROLL | wxTE_MULTILINE,
        wxDefaultValidator, wxT( "OptionsMemo" ) );
    OptionsMemo->SetMaxLength( 0 );
    OptionsMemo->SetFocus();
    OptionsMemo->SetInsertionPointEnd();
    HasChildrenEdit = new wxTextCtrl(
        this, ID_HASCHILDRENEDIT, wxT( "" ), wxPoint( 643, 315 ),
        wxSize( 115, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "HasChildrenEdit" ) );
    PortTypeEdit = new wxTextCtrl(
        this, ID_PORTTYPEEDIT, wxT( "" ), wxPoint( 656, 261 ),
        wxSize( 104, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "PortTypeEdit" ) );
    MultiportEdit = new wxTextCtrl(
        this, ID_MULTIPORTEDIT, wxT( "" ), wxPoint( 656, 235 ),
        wxSize( 104, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "MultiportEdit" ) );
    GenderEdit = new wxTextCtrl(
        this, ID_GENDEREDIT, wxT( "" ), wxPoint( 656, 209 ),
        wxSize( 104, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "GenderEdit" ) );
    InOrOutEdit = new wxTextCtrl(
        this, ID_INOROUTEDIT, wxT( "" ), wxPoint( 656, 183 ),
        wxSize( 104, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "InOrOutEdit" ) );
    WxEdit16 = new wxTextCtrl(
        this, ID_WXEDIT16, wxT( "" ), wxPoint( 599, 60 ),
        wxSize( 158, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "WxEdit16" ) );
    DefaultValueEdit = new wxTextCtrl(
        this, ID_DEFAULTVALUEEDIT, wxT( "" ), wxPoint( 474, 236 ),
        wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "DefaultValueEdit" ) );
    LowerLimitEdit = new wxTextCtrl(
        this, ID_LOWERLIMITEDIT, wxT( "" ), wxPoint( 474, 209 ),
        wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "LowerLimitEdit" ) );
    UpperLimitEdit = new wxTextCtrl(
        this, ID_UPPERLIMITEDIT, wxT( "" ), wxPoint( 474, 183 ),
        wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "UpperLimitEdit" ) );
    EnterableEdit = new wxTextCtrl(
        this, ID_ENTERABLEEDIT, wxT( "" ), wxPoint( 474, 157 ),
        wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "EnterableEdit" ) );
    OutputEdit = new wxTextCtrl(
        this, ID_OUTPUTEDIT, wxT( "" ), wxPoint( 474, 131 ),
        wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "OutputEdit" ) );
    RecordTypeEdit = new wxTextCtrl(
        this, ID_RECORDTYPEEDIT, wxT( "" ), wxPoint( 453, 64 ),
        wxSize( 118, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "RecordTypeEdit" ) );
    OptionListEdit = new wxTextCtrl(
        this, ID_OPTIONLISTEDIT, wxT( "" ), wxPoint( 262, 193 ),
        wxSize( 132, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "OptionListEdit" ) );
    BasisEdit = new wxTextCtrl(
        this, ID_BASISEDIT, wxT( "" ), wxPoint( 262, 165 ),
        wxSize( 132, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "BasisEdit" ) );
    UnitEdit = new wxTextCtrl(
        this, ID_UNITEDIT, wxT( "" ), wxPoint( 262, 138 ),
        wxSize( 132, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "UnitEdit" ) );
    QuantityEdit = new wxTextCtrl(
        this, ID_QUANTITY, wxT( "" ), wxPoint( 262, 108 ),
        wxSize( 132, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "Quantity" ) );
    ValueEdit = new wxTextCtrl(
        this, ID_VALUEEDIT, wxT( "" ), wxPoint( 262, 82 ),
        wxSize( 91, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "ValueEdit" ) );
    ValueEdit->SetEditable( false );
    DimensionEdit = new wxTextCtrl(
        this, ID_DIMENSIONEDIT, wxT( "" ), wxPoint( 270, 38 ),
        wxSize( 121, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "DimensionEdit" ) );
    NodePath = new wxTextCtrl(
        this, ID_NODEPATH, wxT( "" ), wxPoint( 286, 11 ),
        wxSize( 477, 21 ), wxTE_READONLY, wxDefaultValidator,
        wxT( "NodePath" ) );

    CSBox = new wxStaticBox(
        this, ID_CSBOX, wxT( "Completion Status" ), wxPoint( 593, 42 ),
        wxSize( 172, 117 ) );
    CSBox->SetForegroundColour( wxColour( *wxBLACK ) );
    OABox = new wxStaticBox(
        this, ID_OABOX, wxT( "Other Attributes" ), wxPoint( 594, 295 ),
        wxSize( 171, 54 ) );
    OABox->SetForegroundColour( wxColour( *wxBLACK ) );
    FCPBox = new wxStaticBox(
        this, ID_FCPBOX, wxT( "Flowsheet Connectivity Port" ),
        wxPoint( 594, 160 ), wxSize( 171, 130 ) );
    FCPBox->SetForegroundColour( wxColour( *wxBLACK ) );
    AVNBox = new wxStaticBox(
        this, ID_AVNBOX, wxT( "Attributes for Variable Nodes" ),
        wxPoint( 412, 109 ), wxSize( 176, 241 ) );
    AVNBox->SetForegroundColour( wxColour( *wxBLACK ) );
    MARBox = new wxStaticBox(
        this, ID_MARBOX, wxT( "Meta-data Attributes for Records" ),
        wxPoint( 408, 45 ), wxSize( 177, 52 ) );
    MARBox->SetForegroundColour( wxColour( *wxBLACK ) );
    VARBox = new wxStaticBox(
        this, ID_, wxT( "Value Related Attributes" ), wxPoint( 207, 63 ),
        wxSize( 199, 287 ) );
    VARBox->SetForegroundColour( wxColour( *wxBLACK ) );

    wxArrayString arrayStringFor_ParamChoice;
    ParamChoice =
        new wxTreeCtrl( this, ID_PARAMCHOICE,
        wxPoint( 5, 5 ), wxSize( 200, 345 ),
        wxTR_HAS_BUTTONS | wxTR_HIDE_ROOT | wxTR_LINES_AT_ROOT,
        wxDefaultValidator, wxT( "ParamChoice" ) );
    
    m_rootId = ParamChoice->AddRoot( wxT( "Inputs" ), 0, -1, NULL );
    m_prevSelection = m_rootId;
    //ParamChoice->SetSelection( -1 );

    //allow the class to work with blocks and streams
    IsBlock = true;
}

/*void ParamsDlg::OnClose( wxCloseEvent& event )
{
    Destroy();
}*/

//ParamChoiceSelected
void ParamsDlg::ParamChoiceSelected( wxTreeEvent& event )
{
    //get selection
    wxTreeItemId selection = ParamChoice->GetSelection();
    if( m_prevSelection != selection | selection != m_rootId  )
    {
        //get components name
        std::string compName = ConvertUnicode( CompName.c_str() );
        ves::open::xml::CommandPtr
            returnState( new ves::open::xml::Command() );

        //Block - input or output
        if( !DialogType.compare( wxT( "input" ) ) )
        {
            if( IsBlock )
            {
                returnState->SetCommandName( "getInputModuleProperties" );
            }
            else
            {
                returnState->
                    SetCommandName("getStreamInputModuleProperties" );
            }
            ValueEdit->SetEditable( true );
            SetButton->Enable( true );
        }
        else
        {
            if( IsBlock )
            {
                returnState->SetCommandName( "getOutputModuleProperties" );
            }
            else
            {
                returnState->
                    SetCommandName( "getStreamOutputModuleProperties" );
            }
            ValueEdit->SetEditable( false );
            SetButton->Enable( false );
        }

        //create the dvp
        ves::open::xml::DataValuePairPtr
            data( new ves::open::xml::DataValuePair() );
        data->SetData( std::string( "ModuleName" ), compName + "." );
        returnState->AddDataValuePair( data );
        data = ves::open::xml::DataValuePairPtr(
            new ves::open::xml::DataValuePair() );
        
        //get the variable path
        mParentId = ParamChoice->GetItemParent( selection );
        if( mParentId == m_rootId ) //|| selection == m_rootId )
        {
            data->SetData( std::string( "ParamName" ),
                ConvertUnicode( ParamChoice->GetItemText(
                selection ).c_str() ) );
        }
        else
        {
            data->SetData( std::string( "ParamName" ),
                ConvertUnicode( ParamChoice->GetItemText( mParentId ) ) + std::string( "." ) +
                ConvertUnicode( ParamChoice->GetItemText( selection ) ) );
        }
        returnState->AddDataValuePair( data );
        std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
            nodes;
        nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
            ( returnState, "vecommand" ) );

        //create xml
        ves::open::xml::XMLReaderWriter commandWriter;
        std::string status = "returnString";
        commandWriter.UseStandaloneDOMDocumentManager();
        commandWriter.WriteXMLDocument( nodes, status, "Command" );
        wxMessageDialog tempMessageDialog( this, 
            wxString( _("Gathering inputs") ), 
            wxString( _("Message box") ), wxICON_INFORMATION );
        tempMessageDialog.CenterOnParent();
        tempMessageDialog.Show();
        std::string nw_str = serviceList->Query( status );

        ves::open::xml::XMLReaderWriter networkReader;
        networkReader.UseStandaloneDOMDocumentManager();
        networkReader.ReadFromString();
        networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
        std::vector< ves::open::xml::XMLObjectPtr > objectVector =
            networkReader.GetLoadedXMLObjects();
        ves::open::xml::CommandPtr cmd =
            boost::dynamic_pointer_cast<ves::open::xml::Command>
            ( objectVector.at( 0 ) );

        //get all variable components
        unsigned int num = cmd->GetNumberOfDataValuePairs();
        std::vector< std::string > dataName;
        std::vector< std::string > dataValue;
        for( int j = 0; j < static_cast<int>( num ); j++ )
        {
            ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( j );

            //output<<j<<": " << pair->GetDataName().c_str()<<std::endl;
            //if(pair->GetDataName() == "Name")
            if( pair->GetDataName() == "NodePath" )
            {
                NodePath->SetValue( wxString( pair->GetDataString().c_str(),
                    wxConvUTF8 ) );
            }
            //else if(pair->GetDataName()() == "AliasName")
            else if( pair->GetDataName() == "Basis" )
            {
                BasisEdit->SetValue( wxString( pair->GetDataString().c_str(),
                    wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "CompletionStatus" )
            {
                WxEdit16->SetValue( wxString( pair->GetDataString().c_str(),
                    wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "DefaultValue" )
            {
                DefaultValueEdit->SetValue( wxString(
                    pair->GetDataString().c_str(),wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "Gender" )
            {
                GenderEdit->SetValue( wxString( pair->GetDataString().c_str(),
                    wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "InorOut" )
            {
                InOrOutEdit->SetValue( wxString( pair->GetDataString().c_str(),
                    wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "Multiport" )
            {
                MultiportEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "NumChild" )
            {
                std::stringstream conversion;
                unsigned int intValue;
                pair->GetData( intValue );
                conversion << intValue;
                DimensionEdit->SetValue( wxString( conversion.str().c_str(),
                    wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "optionList" )
            {
                OptionListEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "Options" )
            {
                OptionsMemo->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "PhysicalQuantity" )
            {
                QuantityEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "PortType" )
            {
                PortTypeEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "Prompt" )
            {    
                PromptMemo->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "RecordType" )
            {
                RecordTypeEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "UnitOfMeasure" )
            {
                UnitEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "Value" )
            {
                ValueEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "hasChild" )
            {
                HasChildrenEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "isEnterable" )
            {
                EnterableEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "isOutput" )
            {
                OutputEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "upLimit" )
            {
                UpperLimitEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "lowerLimit" )
            {    
                LowerLimitEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
            else if( pair->GetDataName() == "Multiport" )
            {
                MultiportEdit->SetValue( wxString(
                    pair->GetDataString().c_str(), wxConvUTF8 ) );
            }
        }
    }
}

//SetButtonClick
void ParamsDlg::SetButtonClick( wxCommandEvent& event )
{
    std::string compName = ConvertUnicode( CompName.c_str() );
    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "setParam" );

    ves::open::xml::DataValuePairPtr
        moduleName( new ves::open::xml::DataValuePair() );
    moduleName->SetData( "ModuleName", compName );
    returnState->AddDataValuePair( moduleName );

	ves::open::xml::DataValuePairPtr 
		paramName( new ves::open::xml::DataValuePair() );
	if( mParentId == m_rootId )
	{
		paramName->SetData( std::string( "ParamName" ), ConvertUnicode( 
			ParamChoice->GetItemText( 
			ParamChoice->GetSelection() ).c_str() ) );
	}
	else
	{
		paramName->SetData( std::string( "ParamName" ), ConvertUnicode( 
			ParamChoice->GetItemText( mParentId ) + "." +
			ParamChoice->GetItemText( ParamChoice->GetSelection() ).c_str() ) );
	}
    returnState->AddDataValuePair( paramName );

    ves::open::xml::DataValuePairPtr
        paramValue( new ves::open::xml::DataValuePair() );
    paramValue->SetData( "ParamValue", ConvertUnicode(
        ValueEdit->GetValue().c_str() ) );
    returnState->AddDataValuePair( paramValue );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > >
        nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >
        ( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    serviceList->Query( status );
}
void ParamsDlg::AppendList( const char * input )
{
    std::string inputName( input );
    if( inputName.find(".") == std::string::npos )
    {
        m_prevId =
            ParamChoice->AppendItem( m_rootId, wxString( input, wxConvUTF8 ) );
    }
    else
    {
        std::string subInputName = inputName.substr( inputName.find(".")+1 );
        ParamChoice->AppendItem(
            m_prevId, wxString( subInputName.c_str(), wxConvUTF8 ) );
    }
}

void ParamsDlg::SetCompName( const char * name )
{
    this->CompName = wxString( name, wxConvUTF8 );
}

void ParamsDlg::SetServiceList(
    ves::conductor::util::CORBAServiceList * serviceList )
{
    this->serviceList = serviceList;
}

void ParamsDlg::SetDialogType( const char * type )
{
    this->DialogType = wxString( type, wxConvUTF8 );
}

void ParamsDlg::SetIsBlock( bool isBlock )
{
    IsBlock = isBlock;
}
