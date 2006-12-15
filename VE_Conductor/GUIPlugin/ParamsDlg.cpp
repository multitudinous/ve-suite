#include "VE_Conductor/GUIPlugin/ParamsDlg.h"

BEGIN_EVENT_TABLE(ParamsDlg,wxDialog)
	EVT_CLOSE(ParamsDlg::OnClose)
	EVT_BUTTON(ID_SETBUTTON,ParamsDlg::SetButtonClick)
	EVT_CHOICE(ID_PARAMCHOICE,ParamsDlg::ParamChoiceSelected)
END_EVENT_TABLE()

ParamsDlg::ParamsDlg(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

ParamsDlg::~ParamsDlg()
{
} 

void ParamsDlg::CreateGUIControls()
{
	SetTitle(wxT("ParamsDialog"));
	SetIcon(wxNullIcon);
	SetSize(8,8,570,426);
	Center();
	

	SetButton = new wxButton(this, ID_SETBUTTON, wxT("Set"), wxPoint(150,105), wxSize(40,25), 0, wxDefaultValidator, wxT("SetButton"));

	WxMemo3 = new wxTextCtrl(this, ID_WXMEMO3, wxT(""), wxPoint(394,108), wxSize(158,71), wxTE_MULTILINE, wxDefaultValidator, wxT("WxMemo3"));
	WxMemo3->SetMaxLength(0);
	WxMemo3->Enable(false);
	WxMemo3->SetFocus();
	WxMemo3->SetInsertionPointEnd();

	PromptMemo = new wxTextCtrl(this, ID_PROMPTMEMO, wxT(""), wxPoint(269,286), wxSize(103,83), wxVSCROLL | wxTE_MULTILINE, wxDefaultValidator, wxT("PromptMemo"));
	PromptMemo->SetMaxLength(0);
	PromptMemo->Enable(false);
	PromptMemo->SetFocus();
	PromptMemo->SetInsertionPointEnd();

	OptionsMemo = new wxTextCtrl(this, ID_OPTIONSMEMO, wxT(""), wxPoint(67,250), wxSize(121,118), wxVSCROLL | wxTE_MULTILINE, wxDefaultValidator, wxT("OptionsMemo"));
	OptionsMemo->SetMaxLength(0);
	OptionsMemo->Enable(false);
	OptionsMemo->SetFocus();
	OptionsMemo->SetInsertionPointEnd();

	ChildrenLabel = new wxStaticText(this, ID_CHILDRENLABEL, wxT("Children"), wxPoint(392,349), wxDefaultSize, 0, wxT("ChildrenLabel"));

	HasLabel = new wxStaticText(this, ID_HASLABEL, wxT("Has"), wxPoint(392,337), wxDefaultSize, 0, wxT("HasLabel"));

	HasChildrenEdit = new wxTextCtrl(this, ID_HASCHILDRENEDIT, wxT(""), wxPoint(436,340), wxSize(115,21), 0, wxDefaultValidator, wxT("HasChildrenEdit"));
	HasChildrenEdit->Enable(false);
	HasChildrenEdit->SetBackgroundColour(wxColour(*wxWHITE));

	OABox = new wxStaticBox(this, ID_OABOX, wxT("Other Attributes"), wxPoint(389,320), wxSize(171,54));
	OABox->SetForegroundColour(wxColour(*wxBLACK));

	PortTypeEdit = new wxTextCtrl(this, ID_PORTTYPEEDIT, wxT(""), wxPoint(451,286), wxSize(104,21), 0, wxDefaultValidator, wxT("PortTypeEdit"));
	PortTypeEdit->Enable(false);
	PortTypeEdit->SetBackgroundColour(wxColour(*wxWHITE));

	PortTypeLabel = new wxStaticText(this, ID_PORTTYPELABEL, wxT("Port Type"), wxPoint(395,289), wxDefaultSize, 0, wxT("PortTypeLabel"));

	MultiportLabel = new wxStaticText(this, ID_MULTIPORTLABEL, wxT("Multiport"), wxPoint(395,262), wxDefaultSize, 0, wxT("MultiportLabel"));

	MultiportEdit = new wxTextCtrl(this, ID_MULTIPORTEDIT, wxT(""), wxPoint(451,260), wxSize(104,21), 0, wxDefaultValidator, wxT("MultiportEdit"));
	MultiportEdit->Enable(false);
	MultiportEdit->SetBackgroundColour(wxColour(*wxWHITE));

	GenderEdit = new wxTextCtrl(this, ID_GENDEREDIT, wxT(""), wxPoint(451,234), wxSize(104,21), 0, wxDefaultValidator, wxT("GenderEdit"));
	GenderEdit->Enable(false);
	GenderEdit->SetBackgroundColour(wxColour(*wxWHITE));

	GenderLabel = new wxStaticText(this, ID_GENDERLABEL, wxT("Gender"), wxPoint(395,237), wxDefaultSize, 0, wxT("GenderLabel"));

	InOrOutLabel = new wxStaticText(this, ID_INOROUTLABEL, wxT("In or Out"), wxPoint(395,212), wxDefaultSize, 0, wxT("InOrOutLabel"));

	InOrOutEdit = new wxTextCtrl(this, ID_INOROUTEDIT, wxT(""), wxPoint(451,208), wxSize(104,21), 0, wxDefaultValidator, wxT("InOrOutEdit"));
	InOrOutEdit->Enable(false);
	InOrOutEdit->SetBackgroundColour(wxColour(*wxWHITE));

	FCPBox = new wxStaticBox(this, ID_FCPBOX, wxT("Flowsheet Connectivity Port"), wxPoint(389,186), wxSize(171,130));
	FCPBox->SetForegroundColour(wxColour(*wxBLACK));

	TypeLabel = new wxStaticText(this, ID_TYPELABEL, wxT("Type"), wxPoint(209,98), wxDefaultSize, 0, wxT("TypeLabel"));

	WxEdit16 = new wxTextCtrl(this, ID_WXEDIT16, wxT(""), wxPoint(394,85), wxSize(158,21), 0, wxDefaultValidator, wxT("WxEdit16"));
	WxEdit16->Enable(false);
	WxEdit16->SetBackgroundColour(wxColour(*wxWHITE));

	CSBox = new wxStaticBox(this, ID_CSBOX, wxT("Completion Status"), wxPoint(388,67), wxSize(172,117));
	CSBox->SetForegroundColour(wxColour(*wxBLACK));

	DValueLabel = new wxStaticText(this, ID_DVALUELABEL, wxT("Value"), wxPoint(213,272), wxDefaultSize, 0, wxT("DValueLabel"));

	ListLabel = new wxStaticText(this, ID_LISTLABEL, wxT("List"), wxPoint(11,228), wxDefaultSize, 0, wxT("ListLabel"));

	MeasureLabel = new wxStaticText(this, ID_MEASURELABEL, wxT("Measure"), wxPoint(11,173), wxDefaultSize, 0, wxT("MeasureLabel"));

	QuantityLabel = new wxStaticText(this, ID_QUANTITYLABEL, wxT("Quantity"), wxPoint(11,142), wxDefaultSize, 0, wxT("QuantityLabel"));

	PromptLabel = new wxStaticText(this, ID_PROMPT, wxT("Prompt"), wxPoint(212,289), wxDefaultSize, 0, wxT("PromptLabel"));

	DefaultValueEdit = new wxTextCtrl(this, ID_DEFAULTVALUEEDIT, wxT(""), wxPoint(269,261), wxSize(103,21), 0, wxDefaultValidator, wxT("DefaultValueEdit"));
	DefaultValueEdit->Enable(false);

	DefaultLabel = new wxStaticText(this, ID_DEFAULTLABEL, wxT("Default"), wxPoint(213,258), wxDefaultSize, 0, wxT("DefaultLabel"));

	LowerLimitEdit = new wxTextCtrl(this, ID_LOWERLIMITEDIT, wxT(""), wxPoint(269,234), wxSize(103,21), 0, wxDefaultValidator, wxT("LowerLimitEdit"));
	LowerLimitEdit->Enable(false);

	LowerLimitLabel = new wxStaticText(this, ID_LOWERLIMITLABEL, wxT("Lower Limit"), wxPoint(212,237), wxDefaultSize, 0, wxT("LowerLimitLabel"));

	UpperLimitLabel = new wxStaticText(this, ID_UPPERLIMITLABEL, wxT("Upper Limit"), wxPoint(212,210), wxDefaultSize, 0, wxT("UpperLimitLabel"));

	UpperLimitEdit = new wxTextCtrl(this, ID_UPPERLIMITEDIT, wxT(""), wxPoint(269,208), wxSize(103,21), 0, wxDefaultValidator, wxT("UpperLimitEdit"));
	UpperLimitEdit->Enable(false);

	EnterableEdit = new wxTextCtrl(this, ID_ENTERABLEEDIT, wxT(""), wxPoint(269,182), wxSize(103,21), 0, wxDefaultValidator, wxT("EnterableEdit"));
	EnterableEdit->Enable(false);

	EnterableLabel = new wxStaticText(this, ID_ENTERABLELABEL, wxT("Enterable"), wxPoint(212,185), wxDefaultSize, 0, wxT("EnterableLabel"));

	OutputLabel = new wxStaticText(this, ID_OUTPUTLABEL, wxT("Output"), wxPoint(212,159), wxDefaultSize, 0, wxT("OutputLabel"));

	OutputEdit = new wxTextCtrl(this, ID_OUTPUTEDIT, wxT(""), wxPoint(269,156), wxSize(103,21), 0, wxDefaultValidator, wxT("OutputEdit"));
	OutputEdit->Enable(false);

	AVNBox = new wxStaticBox(this, ID_AVNBOX, wxT("Attributes for Variable Nodes"), wxPoint(207,134), wxSize(176,241));
	AVNBox->SetForegroundColour(wxColour(*wxBLACK));

	RecordLabel = new wxStaticText(this, ID_RECORDLABEL, wxT("Record"), wxPoint(209,86), wxDefaultSize, 0, wxT("RecordLabel"));

	RecordTypeEdit = new wxTextCtrl(this, ID_RECORDTYPEEDIT, wxT(""), wxPoint(248,89), wxSize(118,21), 0, wxDefaultValidator, wxT("RecordTypeEdit"));
	RecordTypeEdit->Enable(false);
	RecordTypeEdit->SetBackgroundColour(wxColour(*wxWHITE));

	MARBox = new wxStaticBox(this, ID_MARBOX, wxT("Meta-data Attributes for Records"), wxPoint(203,70), wxSize(177,52));
	MARBox->SetForegroundColour(wxColour(*wxBLACK));

	OptionsLabel = new wxStaticText(this, ID_OPTIONSLABEL, wxT("Options"), wxPoint(11,250), wxDefaultSize, 0, wxT("OptionsLabel"));

	OptionLabel = new wxStaticText(this, ID_OPTIONLABEL, wxT("Option"), wxPoint(11,216), wxDefaultSize, 0, wxT("OptionLabel"));

	OptionListEdit = new wxTextCtrl(this, ID_OPTIONLISTEDIT, wxT(""), wxPoint(57,218), wxSize(132,21), 0, wxDefaultValidator, wxT("OptionListEdit"));
	OptionListEdit->Enable(false);
	OptionListEdit->SetBackgroundColour(wxColour(*wxWHITE));

	BasisLabel = new wxStaticText(this, ID_BASISLABEL, wxT("Basis"), wxPoint(11,191), wxDefaultSize, 0, wxT("BasisLabel"));

	BasisEdit = new wxTextCtrl(this, ID_BASISEDIT, wxT(""), wxPoint(57,190), wxSize(132,21), 0, wxDefaultValidator, wxT("BasisEdit"));
	BasisEdit->Enable(false);
	BasisEdit->SetBackgroundColour(wxColour(*wxWHITE));

	UnitLabel = new wxStaticText(this, ID_UNITLABEL, wxT("Unit of"), wxPoint(11,159), wxDefaultSize, 0, wxT("UnitLabel"));

	UnitEdit = new wxTextCtrl(this, ID_UNITEDIT, wxT(""), wxPoint(57,163), wxSize(132,21), 0, wxDefaultValidator, wxT("UnitEdit"));
	UnitEdit->Enable(false);
	UnitEdit->SetBackgroundColour(wxColour(*wxWHITE));

	PhysicalLabel = new wxStaticText(this, ID_PHYSICALLABEL, wxT("Physical"), wxPoint(11,129), wxDefaultSize, 0, wxT("PhysicalLabel"));

	QuantityEdit = new wxTextCtrl(this, ID_QUANTITY, wxT(""), wxPoint(57,133), wxSize(132,21), 0, wxDefaultValidator, wxT("Quantity"));
	QuantityEdit->Enable(false);
	QuantityEdit->SetBackgroundColour(wxColour(*wxWHITE));

	ValueEdit = new wxTextCtrl(this, ID_VALUEEDIT, wxT(""), wxPoint(57,107), wxSize(91,21), 0, wxDefaultValidator, wxT("ValueEdit"));

	ValueLabel = new wxStaticText(this, ID_VALUELABEL, wxT("Value"), wxPoint(11,111), wxDefaultSize, 0, wxT("ValueLabel"));

	VARBox = new wxStaticBox(this, ID_, wxT("Value Related Attributes"), wxPoint(2,88), wxSize(199,287));
	VARBox->SetForegroundColour(wxColour(*wxBLACK));

	DimensionLabel = new wxStaticText(this, ID_DIMENSIONLABEL, wxT("Dimension"), wxPoint(12,66), wxDefaultSize, 0, wxT("DimensionLabel"));

	DimensionEdit = new wxTextCtrl(this, ID_DIMENSIONEDIT, wxT(""), wxPoint(65,63), wxSize(121,21), 0, wxDefaultValidator, wxT("DimensionEdit"));
	DimensionEdit->Enable(false);
	DimensionEdit->SetBackgroundColour(wxColour(*wxWHITE));

	wxArrayString arrayStringFor_ParamChoice;
	ParamChoice = new wxChoice(this, ID_PARAMCHOICE, wxPoint(166,8), wxSize(211,21), arrayStringFor_ParamChoice, 0, wxDefaultValidator, wxT("ParamChoice"));
	ParamChoice->SetSelection(-1);

	NodePathLabel = new wxStaticText(this, ID_NODEPATHLABEL, wxT("Path to Node"), wxPoint(9,37), wxDefaultSize, 0, wxT("NodePathLabel"));

	NodePath = new wxTextCtrl(this, ID_NODEPATH, wxT(""), wxPoint(81,36), wxSize(477,21), 0, wxDefaultValidator, wxT("NodePath"));
	NodePath->Enable(false);
	NodePath->SetBackgroundColour(wxColour(*wxWHITE));

	ParameterLabel = new wxStaticText(this, ID_PARAMETERLABEL, wxT("Parameter"), wxPoint(100,10), wxDefaultSize, 0, wxT("ParameterLabel"));
	ParameterLabel->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxBOLD, FALSE));
	////GUI Items Creation End
}

void ParamsDlg::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}

//ParamChoiceSelected
void ParamsDlg::ParamChoiceSelected(wxCommandEvent& event )
{
	std::string compName = CompName.c_str();

	VE_XML::Command returnState;
	//if(DialogType.c_str() == "input")
		returnState.SetCommandName("getInputModuleProperties");
	//else
	//	returnState.SetCommandName("getOutputModuleProperties");
	VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
	data->SetData(std::string("ModuleName"), compName);
	data = returnState.GetDataValuePair(-1);
	data->SetData(std::string("ParamName"), ParamChoice->GetStringSelection().c_str());
	
	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
	
	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
	std::string nw_str = serviceList->Query( status );
	std::ofstream output("packet.txt");
	output<<nw_str.c_str()<<std::endl;
	output.close();
	VE_XML::XMLReaderWriter networkReader;
	networkReader.UseStandaloneDOMDocumentManager();
	networkReader.ReadFromString();
	networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
	std::vector< VE_XML::XMLObject* > objectVector = networkReader.GetLoadedXMLObjects();
	VE_XML::Command* cmd = dynamic_cast< VE_XML::Command* >( objectVector.at( 0 ) );

	unsigned int num = cmd->GetNumberOfDataValuePairs();		
	std::vector< std::string > dataName;
	std::vector< std::string > dataValue;
	for(int j = 0; j < num; j++)
	{
		VE_XML::DataValuePair * pair = cmd->GetDataValuePair(j);
		//if(pair->GetDataName() == "Name")
		if(pair->GetDataName() == "NodePath")
			NodePath->SetValue(pair->GetDataString().c_str());
		//else if(pair->GetDataName()() == "AliasName")
		else if(pair->GetDataName() == "Basis")
			BasisEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "CompletionStatus")
			WxEdit16->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "DefaultValue")
			DefaultValueEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "Gender")
			GenderEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "InorOut")
			InOrOutEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "Multiport")
			MultiportEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "NumChild")
			DimensionEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "optionList")
			OptionListEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "Options")
			OptionsMemo->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "PhysicalQuantity")
			QuantityEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "PortType")
			PortTypeEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "Prompt")
			PromptMemo->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "RecordType")
			RecordTypeEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "UnitOfMeasure")
			UnitEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "Value")
			ValueEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "hasChild")
			HasChildrenEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "isEnterable")
			EnterableEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "isOutput")
			OutputEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "upLimit")
			UpperLimitEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "lowerLimit")
			LowerLimitEdit->SetValue(pair->GetDataString().c_str());
		else if(pair->GetDataName() == "Multiport")
			MultiportEdit->SetValue(pair->GetDataString().c_str());
	}
}

//SetButtonClick
void ParamsDlg::SetButtonClick(wxCommandEvent& event)
{	
	std::string compName = CompName.c_str();
	VE_XML::Command returnState;
	returnState.SetCommandName("setParam");
	VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
	data->SetData("ModuleName", compName);
	data = returnState.GetDataValuePair(-1);
	data->SetData("ParamName", ParamChoice->GetStringSelection().c_str());
	data = returnState.GetDataValuePair(-1);
	data->SetData("ParamValue", ValueEdit->GetValue().c_str());

	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
	
	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
	serviceList->Query( status );
}
void ParamsDlg::AppendList(const char * input)
{
	ParamChoice->Append(wxT(input));
}

void ParamsDlg::SetCompName(const char * name)
{
	this->CompName = wxT(name);
}

void ParamsDlg::SetServiceList(VE_Conductor::CORBAServiceList * serviceList)
{
	this->serviceList = serviceList;
}

void ParamsDlg::SetDialogType(const char * type)
{
	this->DialogType = wxT(type);
}