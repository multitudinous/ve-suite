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
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( ParamsDlg, wxDialog )
    //EVT_CLOSE( ParamsDlg::OnClose )
    EVT_BUTTON( ID_SETBUTTON, ParamsDlg::SetButtonClick )
    EVT_CHOICE( ID_PARAMCHOICE, ParamsDlg::ParamChoiceSelected )
END_EVENT_TABLE()

ParamsDlg::ParamsDlg( wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style )
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
    SetSize( 8, 8, 570, 426 );
    Center();

    SetButton = new wxButton( this, ID_SETBUTTON, wxT( "Set" ), wxPoint( 150, 105 ), wxSize( 40, 25 ), 0, wxDefaultValidator, wxT( "SetButton" ) );
    SetButton->Enable( false );

    ChildrenLabel = new wxStaticText( this, ID_CHILDRENLABEL, wxT( "Children" ), wxPoint( 392, 349 ), wxDefaultSize, 0, wxT( "ChildrenLabel" ) );
    HasLabel = new wxStaticText( this, ID_HASLABEL, wxT( "Has" ), wxPoint( 392, 337 ), wxDefaultSize, 0, wxT( "HasLabel" ) );
    PortTypeLabel = new wxStaticText( this, ID_PORTTYPELABEL, wxT( "Port Type" ), wxPoint( 395, 289 ), wxDefaultSize, 0, wxT( "PortTypeLabel" ) );
    MultiportLabel = new wxStaticText( this, ID_MULTIPORTLABEL, wxT( "Multiport" ), wxPoint( 395, 262 ), wxDefaultSize, 0, wxT( "MultiportLabel" ) );
    GenderLabel = new wxStaticText( this, ID_GENDERLABEL, wxT( "Gender" ), wxPoint( 395, 237 ), wxDefaultSize, 0, wxT( "GenderLabel" ) );
    InOrOutLabel = new wxStaticText( this, ID_INOROUTLABEL, wxT( "In or Out" ), wxPoint( 395, 212 ), wxDefaultSize, 0, wxT( "InOrOutLabel" ) );
    TypeLabel = new wxStaticText( this, ID_TYPELABEL, wxT( "Type" ), wxPoint( 209, 98 ), wxDefaultSize, 0, wxT( "TypeLabel" ) );
    DValueLabel = new wxStaticText( this, ID_DVALUELABEL, wxT( "Value" ), wxPoint( 213, 272 ), wxDefaultSize, 0, wxT( "DValueLabel" ) );
    ListLabel = new wxStaticText( this, ID_LISTLABEL, wxT( "List" ), wxPoint( 11, 228 ), wxDefaultSize, 0, wxT( "ListLabel" ) );
    MeasureLabel = new wxStaticText( this, ID_MEASURELABEL, wxT( "Measure" ), wxPoint( 11, 173 ), wxDefaultSize, 0, wxT( "MeasureLabel" ) );
    QuantityLabel = new wxStaticText( this, ID_QUANTITYLABEL, wxT( "Quantity" ), wxPoint( 11, 142 ), wxDefaultSize, 0, wxT( "QuantityLabel" ) );
    PromptLabel = new wxStaticText( this, ID_PROMPT, wxT( "Prompt" ), wxPoint( 212, 289 ), wxDefaultSize, 0, wxT( "PromptLabel" ) );
    DefaultLabel = new wxStaticText( this, ID_DEFAULTLABEL, wxT( "Default" ), wxPoint( 213, 258 ), wxDefaultSize, 0, wxT( "DefaultLabel" ) );
    LowerLimitLabel = new wxStaticText( this, ID_LOWERLIMITLABEL, wxT( "Lower Limit" ), wxPoint( 212, 237 ), wxDefaultSize, 0, wxT( "LowerLimitLabel" ) );
    UpperLimitLabel = new wxStaticText( this, ID_UPPERLIMITLABEL, wxT( "Upper Limit" ), wxPoint( 212, 210 ), wxDefaultSize, 0, wxT( "UpperLimitLabel" ) );
    EnterableLabel = new wxStaticText( this, ID_ENTERABLELABEL, wxT( "Enterable" ), wxPoint( 212, 185 ), wxDefaultSize, 0, wxT( "EnterableLabel" ) );
    OutputLabel = new wxStaticText( this, ID_OUTPUTLABEL, wxT( "Output" ), wxPoint( 212, 159 ), wxDefaultSize, 0, wxT( "OutputLabel" ) );
    RecordLabel = new wxStaticText( this, ID_RECORDLABEL, wxT( "Record" ), wxPoint( 209, 86 ), wxDefaultSize, 0, wxT( "RecordLabel" ) );
    OptionsLabel = new wxStaticText( this, ID_OPTIONSLABEL, wxT( "Options" ), wxPoint( 11, 250 ), wxDefaultSize, 0, wxT( "OptionsLabel" ) );
    OptionLabel = new wxStaticText( this, ID_OPTIONLABEL, wxT( "Option" ), wxPoint( 11, 216 ), wxDefaultSize, 0, wxT( "OptionLabel" ) );
    BasisLabel = new wxStaticText( this, ID_BASISLABEL, wxT( "Basis" ), wxPoint( 11, 191 ), wxDefaultSize, 0, wxT( "BasisLabel" ) );
    UnitLabel = new wxStaticText( this, ID_UNITLABEL, wxT( "Unit of" ), wxPoint( 11, 159 ), wxDefaultSize, 0, wxT( "UnitLabel" ) );
    PhysicalLabel = new wxStaticText( this, ID_PHYSICALLABEL, wxT( "Physical" ), wxPoint( 11, 129 ), wxDefaultSize, 0, wxT( "PhysicalLabel" ) );
    ValueLabel = new wxStaticText( this, ID_VALUELABEL, wxT( "Value" ), wxPoint( 11, 111 ), wxDefaultSize, 0, wxT( "ValueLabel" ) );
    DimensionLabel = new wxStaticText( this, ID_DIMENSIONLABEL, wxT( "Dimension" ), wxPoint( 12, 66 ), wxDefaultSize, 0, wxT( "DimensionLabel" ) );
    NodePathLabel = new wxStaticText( this, ID_NODEPATHLABEL, wxT( "Path to Node" ), wxPoint( 9, 37 ), wxDefaultSize, 0, wxT( "NodePathLabel" ) );
    ParameterLabel = new wxStaticText( this, ID_PARAMETERLABEL, wxT( "Parameter" ), wxPoint( 100, 10 ), wxDefaultSize, 0, wxT( "ParameterLabel" ) );
    ParameterLabel->SetFont( wxFont( 8, wxSWISS, wxNORMAL, wxBOLD, FALSE ) );

    WxMemo3 = new wxTextCtrl( this, ID_WXMEMO3, wxT( "" ), wxPoint( 394, 108 ), wxSize( 158, 71 ), wxTE_READONLY | wxTE_MULTILINE, wxDefaultValidator, wxT( "WxMemo3" ) );
    WxMemo3->SetMaxLength( 0 );
    WxMemo3->SetFocus();
    WxMemo3->SetInsertionPointEnd();
    PromptMemo = new wxTextCtrl( this, ID_PROMPTMEMO, wxT( "" ), wxPoint( 269, 286 ), wxSize( 103, 83 ), wxTE_READONLY | wxVSCROLL | wxTE_MULTILINE, wxDefaultValidator, wxT( "PromptMemo" ) );
    PromptMemo->SetMaxLength( 0 );
    PromptMemo->SetFocus();
    PromptMemo->SetInsertionPointEnd();
    OptionsMemo = new wxTextCtrl( this, ID_OPTIONSMEMO, wxT( "" ), wxPoint( 67, 250 ), wxSize( 121, 118 ), wxTE_READONLY | wxVSCROLL | wxTE_MULTILINE, wxDefaultValidator, wxT( "OptionsMemo" ) );
    OptionsMemo->SetMaxLength( 0 );
    OptionsMemo->SetFocus();
    OptionsMemo->SetInsertionPointEnd();
    HasChildrenEdit = new wxTextCtrl( this, ID_HASCHILDRENEDIT, wxT( "" ), wxPoint( 436, 340 ), wxSize( 115, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "HasChildrenEdit" ) );
    PortTypeEdit = new wxTextCtrl( this, ID_PORTTYPEEDIT, wxT( "" ), wxPoint( 451, 286 ), wxSize( 104, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "PortTypeEdit" ) );
    MultiportEdit = new wxTextCtrl( this, ID_MULTIPORTEDIT, wxT( "" ), wxPoint( 451, 260 ), wxSize( 104, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "MultiportEdit" ) );
    GenderEdit = new wxTextCtrl( this, ID_GENDEREDIT, wxT( "" ), wxPoint( 451, 234 ), wxSize( 104, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "GenderEdit" ) );
    InOrOutEdit = new wxTextCtrl( this, ID_INOROUTEDIT, wxT( "" ), wxPoint( 451, 208 ), wxSize( 104, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "InOrOutEdit" ) );
    WxEdit16 = new wxTextCtrl( this, ID_WXEDIT16, wxT( "" ), wxPoint( 394, 85 ), wxSize( 158, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "WxEdit16" ) );
    DefaultValueEdit = new wxTextCtrl( this, ID_DEFAULTVALUEEDIT, wxT( "" ), wxPoint( 269, 261 ), wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "DefaultValueEdit" ) );
    LowerLimitEdit = new wxTextCtrl( this, ID_LOWERLIMITEDIT, wxT( "" ), wxPoint( 269, 234 ), wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "LowerLimitEdit" ) );
    UpperLimitEdit = new wxTextCtrl( this, ID_UPPERLIMITEDIT, wxT( "" ), wxPoint( 269, 208 ), wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "UpperLimitEdit" ) );
    EnterableEdit = new wxTextCtrl( this, ID_ENTERABLEEDIT, wxT( "" ), wxPoint( 269, 182 ), wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "EnterableEdit" ) );
    OutputEdit = new wxTextCtrl( this, ID_OUTPUTEDIT, wxT( "" ), wxPoint( 269, 156 ), wxSize( 103, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "OutputEdit" ) );
    RecordTypeEdit = new wxTextCtrl( this, ID_RECORDTYPEEDIT, wxT( "" ), wxPoint( 248, 89 ), wxSize( 118, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "RecordTypeEdit" ) );
    OptionListEdit = new wxTextCtrl( this, ID_OPTIONLISTEDIT, wxT( "" ), wxPoint( 57, 218 ), wxSize( 132, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "OptionListEdit" ) );
    BasisEdit = new wxTextCtrl( this, ID_BASISEDIT, wxT( "" ), wxPoint( 57, 190 ), wxSize( 132, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "BasisEdit" ) );
    UnitEdit = new wxTextCtrl( this, ID_UNITEDIT, wxT( "" ), wxPoint( 57, 163 ), wxSize( 132, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "UnitEdit" ) );
    QuantityEdit = new wxTextCtrl( this, ID_QUANTITY, wxT( "" ), wxPoint( 57, 133 ), wxSize( 132, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "Quantity" ) );
    ValueEdit = new wxTextCtrl( this, ID_VALUEEDIT, wxT( "" ), wxPoint( 57, 107 ), wxSize( 91, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "ValueEdit" ) );
    ValueEdit->SetEditable( false );
    DimensionEdit = new wxTextCtrl( this, ID_DIMENSIONEDIT, wxT( "" ), wxPoint( 65, 63 ), wxSize( 121, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "DimensionEdit" ) );
    NodePath = new wxTextCtrl( this, ID_NODEPATH, wxT( "" ), wxPoint( 81, 36 ), wxSize( 477, 21 ), wxTE_READONLY, wxDefaultValidator, wxT( "NodePath" ) );

    CSBox = new wxStaticBox( this, ID_CSBOX, wxT( "Completion Status" ), wxPoint( 388, 67 ), wxSize( 172, 117 ) );
    CSBox->SetForegroundColour( wxColour( *wxBLACK ) );
    OABox = new wxStaticBox( this, ID_OABOX, wxT( "Other Attributes" ), wxPoint( 389, 320 ), wxSize( 171, 54 ) );
    OABox->SetForegroundColour( wxColour( *wxBLACK ) );
    FCPBox = new wxStaticBox( this, ID_FCPBOX, wxT( "Flowsheet Connectivity Port" ), wxPoint( 389, 186 ), wxSize( 171, 130 ) );
    FCPBox->SetForegroundColour( wxColour( *wxBLACK ) );
    AVNBox = new wxStaticBox( this, ID_AVNBOX, wxT( "Attributes for Variable Nodes" ), wxPoint( 207, 134 ), wxSize( 176, 241 ) );
    AVNBox->SetForegroundColour( wxColour( *wxBLACK ) );
    MARBox = new wxStaticBox( this, ID_MARBOX, wxT( "Meta-data Attributes for Records" ), wxPoint( 203, 70 ), wxSize( 177, 52 ) );
    MARBox->SetForegroundColour( wxColour( *wxBLACK ) );
    VARBox = new wxStaticBox( this, ID_, wxT( "Value Related Attributes" ), wxPoint( 2, 88 ), wxSize( 199, 287 ) );
    VARBox->SetForegroundColour( wxColour( *wxBLACK ) );

    wxArrayString arrayStringFor_ParamChoice;
    ParamChoice = new wxChoice( this, ID_PARAMCHOICE, wxPoint( 166, 8 ), wxSize( 211, 21 ), arrayStringFor_ParamChoice, 0, wxDefaultValidator, wxT( "ParamChoice" ) );
    ParamChoice->SetSelection( -1 );

    //allow the class to work with blocks and streams
    IsBlock = true;
}

/*void ParamsDlg::OnClose( wxCloseEvent& event )
{
    Destroy();
}*/

//ParamChoiceSelected
void ParamsDlg::ParamChoiceSelected( wxCommandEvent& event )
{
    std::string compName = ConvertUnicode( CompName.c_str() );

    ves::open::xml::Command returnState;

    //Block
    if( !DialogType.compare( wxT( "input" ) ) )
    {
        if( IsBlock )
            returnState.SetCommandName( "getInputModuleProperties" );
        else
            returnState.SetCommandName( "getStreamInputModuleProperties" );
        ValueEdit->SetEditable( true );
        SetButton->Enable( true );
    }
    else
    {
        if( IsBlock )
            returnState.SetCommandName( "getOutputModuleProperties" );
        else
            returnState.SetCommandName( "getStreamOutputModuleProperties" );
        ValueEdit->SetEditable( false );
        SetButton->Enable( false );
    }


    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState.AddDataValuePair( data );

    data = new ves::open::xml::DataValuePair();
    data->SetData( std::string( "ParamName" ), ConvertUnicode( ParamChoice->GetStringSelection().c_str() ) );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( &returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    std::string nw_str = serviceList->Query( status );
    //std::ofstream output("packet.txt");
    //output<<nw_str.c_str()<<std::endl;
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd = objectVector.at( 0 );

    unsigned int num = cmd->GetNumberOfDataValuePairs();
    std::vector< std::string > dataName;
    std::vector< std::string > dataValue;
    //output << "loop" << std::endl;
    for( int j = 0; j < static_cast<int>( num ); j++ )
    {
        ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( j );

        //output<<j<<": " << pair->GetDataName().c_str()<<std::endl;
        //if(pair->GetDataName() == "Name")
        if( pair->GetDataName() == "NodePath" )
            NodePath->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        //else if(pair->GetDataName()() == "AliasName")
        else if( pair->GetDataName() == "Basis" )
            BasisEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "CompletionStatus" )
            WxEdit16->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "DefaultValue" )
            DefaultValueEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "Gender" )
            GenderEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "InorOut" )
            InOrOutEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "Multiport" )
            MultiportEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "NumChild" )
        {
            std::stringstream conversion;
            unsigned int intValue;
            pair->GetData( intValue );
            conversion << intValue;
            DimensionEdit->SetValue( wxString( conversion.str().c_str(), wxConvUTF8 ) );
            //DimensionEdit->SetValue(pair->GetDataString().c_str());
        }
        else if( pair->GetDataName() == "optionList" )
            OptionListEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "Options" )
            OptionsMemo->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "PhysicalQuantity" )
            QuantityEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "PortType" )
            PortTypeEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "Prompt" )
            PromptMemo->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "RecordType" )
            RecordTypeEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "UnitOfMeasure" )
            UnitEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "Value" )
            ValueEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "hasChild" )
            HasChildrenEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "isEnterable" )
            EnterableEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "isOutput" )
            OutputEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "upLimit" )
            UpperLimitEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "lowerLimit" )
            LowerLimitEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
        else if( pair->GetDataName() == "Multiport" )
            MultiportEdit->SetValue( wxString( pair->GetDataString().c_str(), wxConvUTF8 ) );
    }
    //output.close();
}

//SetButtonClick
void ParamsDlg::SetButtonClick( wxCommandEvent& event )
{
    std::string compName = ConvertUnicode( CompName.c_str() );
    ves::open::xml::Command returnState;
    returnState.SetCommandName( "setParam" );

    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( "ModuleName", compName );
    returnState.AddDataValuePair( data );

    data = new ves::open::xml::DataValuePair();
    data->SetData( "ParamName", ConvertUnicode( ParamChoice->GetStringSelection().c_str() ) );
    returnState.AddDataValuePair( data );

    data = new ves::open::xml::DataValuePair();
    data->SetData( "ParamValue", ConvertUnicode( ValueEdit->GetValue().c_str() ) );
    returnState.AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( &returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    serviceList->Query( status );
}
void ParamsDlg::AppendList( const char * input )
{
    ParamChoice->Append( wxString( input, wxConvUTF8 ) );
}

void ParamsDlg::SetCompName( const char * name )
{
    this->CompName = wxString( name, wxConvUTF8 );
}

void ParamsDlg::SetServiceList( ves::conductor::util::CORBAServiceList * serviceList )
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
