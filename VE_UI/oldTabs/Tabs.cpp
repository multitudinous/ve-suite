#include "Tabs.h"
#include "wx/imaglist.h"
#include "wx/artprov.h"

IMPLEMENT_APP(MyApp)

BEGIN_EVENT_TABLE(Tabs, wxNotebook)
  EVT_RADIOBOX(R_CATEGORY, Tabs::OnCategory)
  EVT_RADIOBOX(R_CONTOUR_TYPE, Tabs::OnContour)
  EVT_RADIOBOX(R_DIRECTION, Tabs::OnDirection)
  EVT_RADIOBUTTON(PRECOMP, Tabs::OnPreComp)
  EVT_RADIOBUTTON(SINGLE, Tabs::OnSingle)
  EVT_CHECKBOX(NEAREST, Tabs::OnNearest)
  EVT_BUTTON(UPDATE, Tabs::OnUpdate)
  EVT_CHECKBOX(BLUE_MENU, Tabs::OnBlueMenu)
  EVT_CHECKBOX(SCALAR_BAR, Tabs::OnScalarBar)
  EVT_BUTTON(RECORD, Tabs::OnRecord)
  EVT_BUTTON(EXIT, Tabs::OnExit)
END_EVENT_TABLE()

bool MyApp::OnInit()
{
    // Create the main window
    MyFrame *frame = new MyFrame( wxT("WX Client for VE Suite") );
	 InitObserver();

    // Problem with generic wxNotebook implementation whereby it doesn't size
    // properly unless you set the size again
#if defined(__WIN16__) || defined(__WXMOTIF__)
    int width, height;
    frame->GetSize(& width, & height);
    frame->SetSize(-1, -1, width, height);
#endif

    frame->Show();

    return TRUE;
}

void MyApp::InitObserver()
{
}

Tabs::Tabs(wxWindow *parent, wxWindowID id,
	   const wxPoint& pos , 
	   const wxSize& size , 
	   long style)
  : wxNotebook(parent, id, pos, size, style)
{
}

wxPanel *Tabs::CreateFirstPage()
{
  wxPanel *panel = new wxPanel(this);

  //Set up the strings in the radio box
  wxString category[] = { wxT("Contour"), wxT("Warped Contour"), wxT("Vector"), wxT("Isosurface"), wxT("PIV_Image"), wxT("Polydata")};

  wxString contour_type[] = { wxT("Graduated"), wxT("Banded"), wxT("Lined")};
  
  wxString direction[] = { wxT("X"), wxT("Y"), wxT("Z"), wxT("By wand")};
  
  //Set up the sizer so we can lay things well
  //The panel will be four rows from top to bottom
  wxBoxSizer *top_sizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *first_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *second_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *third_row = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *forth_row = new wxBoxSizer(wxHORIZONTAL);

  top_sizer->Add(first_row, 1, wxALIGN_LEFT); //
  top_sizer->Add(second_row, 1, wxALIGN_LEFT);
  top_sizer->Add(third_row, 1, wxALIGN_LEFT);
  top_sizer->Add(forth_row, 0, wxALIGN_LEFT);

  //Instantiate the radio box of the first row
  R_Category  = new wxRadioBox(panel, R_CATEGORY, wxT("Category"), wxDefaultPosition, wxDefaultSize, 6, category, 3, wxRA_SPECIFY_COLS);

  R_Contour_type = new wxRadioBox(panel, R_CONTOUR_TYPE, wxT("Conour Type"), wxDefaultPosition, wxDefaultSize, 3, contour_type, 1, wxRA_SPECIFY_COLS);

  first_row->Add(R_Category, 3, wxALIGN_LEFT);
  first_row->Add(R_Contour_type, 1, wxALIGN_RIGHT);

  //The second row
  
  R_Direction = new wxRadioBox(panel, R_DIRECTION, wxT("Direction"), wxDefaultPosition, wxDefaultSize, 4, direction, 1, wxRA_SPECIFY_COLS);

  wxStaticBox* typebox = new wxStaticBox(panel, -1, wxT("Type"));
  wxStaticBoxSizer *typebox_sizer = new wxStaticBoxSizer(typebox, wxVERTICAL);

  R_precomp_but = new wxRadioButton(panel, PRECOMP, wxT("All precomputed surfaces"), wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  R_single_but = new wxRadioButton(panel, SINGLE, wxT("Specify a single plane"));

  C_cycle_but = new wxCheckBox(panel, CYCLE, wxT("Cycle precomputed surfaces"));
  C_cycle_but->Enable(false);
  
  C_nearest_but = new wxCheckBox(panel, NEAREST, wxT("Use nearest precomputed plane"));

  typebox_sizer->Add(R_precomp_but, 1, wxALIGN_LEFT);
  typebox_sizer->Add(C_cycle_but, 1, wxALIGN_RIGHT);
  typebox_sizer->Add(R_single_but, 1, wxALIGN_LEFT);
  typebox_sizer->Add(C_nearest_but, 1, wxALIGN_RIGHT);

  second_row->Add(R_Direction, 1, wxALIGN_LEFT);
  second_row->Add(typebox_sizer, 2, wxALIGN_RIGHT);

  //The third row
  
  wxSize slidesize(300, 50);

  S_value = new wxSlider(panel, SLIDER, 0, 0, 100, wxDefaultPosition, slidesize, wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);

  B_update = new wxButton(panel, UPDATE, wxT("Update"));

  third_row->Add(S_value, 5, wxALIGN_LEFT);
  third_row->Add(B_update, 0, wxALIGN_RIGHT);

  //The forth row

  C_blue_menu = new wxCheckBox(panel, BLUE_MENU, wxT("blue menu"));
  
  C_scalar_bar = new wxCheckBox(panel, SCALAR_BAR, wxT("scalar bar"));

  B_record = new wxButton(panel, RECORD, wxT("record scene"));
  
  B_clear = new wxButton(panel, CLEAR, wxT("clear all"));
  
  B_exit = new wxButton(panel, EXIT, wxT("exit"));

  
  forth_row->Add(C_blue_menu, 0, wxALIGN_CENTER_HORIZONTAL);
  forth_row->Add(C_scalar_bar, 0, wxALIGN_CENTER_HORIZONTAL);
  forth_row->Add(B_record, 0, wxALIGN_CENTER_HORIZONTAL);
  forth_row->Add(B_clear, 0, wxALIGN_CENTER_HORIZONTAL);
  forth_row->Add(B_exit, 0, wxALIGN_CENTER_HORIZONTAL);
  
  panel->SetAutoLayout( true );
  panel->SetSizer(top_sizer); 

  return panel;
}

wxPanel *Tabs::CreateSecondPage()
{
  wxPanel *panel = new wxPanel(this);

  new wxStaticText(panel, -1, _T("This is the second page"));
  return panel;
}


void Tabs::OnCategory(wxCommandEvent& event)
{
  wxMessageBox(R_Category->GetStringSelection(), _T("RadioBox!"));
}

void Tabs::OnContour(wxCommandEvent& event)
{
  wxMessageBox(R_Contour_type->GetStringSelection(), _T("RadioBox!"));
}

void Tabs::OnDirection(wxCommandEvent& event)
{
  wxMessageBox(R_Direction->GetStringSelection(), _T("RadioBox!"));
}

void Tabs::OnPreComp(wxCommandEvent& event)
{
  if (R_precomp_but->GetValue())
    wxMessageBox(_T("single plane selected!"), _T("RadioButton!"));
  else
    wxMessageBox(_T("single plane unselected!"), _T("RadioButton!"));
}

void Tabs::OnSingle(wxCommandEvent& event)
{
  if (R_single_but->GetValue())
    wxMessageBox(_T("single plane selected!"), _T("RadioButton!"));
  else
    wxMessageBox(_T("single plane unselected!"), _T("RadioButton!"));
}

void Tabs::OnNearest(wxCommandEvent& event)
{
  if (C_nearest_but->GetValue())
    wxMessageBox(_T("nearest plane box checked!"), _T("CheckBox!"));
  else
    wxMessageBox(_T("nearest plane box unchecked!"), _T("CheckBox!"));
}

void Tabs::OnUpdate(wxCommandEvent& event)
{
  int cur_pos;
  char s[256];
  
  cur_pos = S_value->GetValue();
  sprintf(s, "Current Slider value: %d !\n", cur_pos);
  wxMessageBox(wxString(s)+_T("Update button clicked!"), _T("Button!"));
}

void Tabs::OnBlueMenu(wxCommandEvent& event)
{
  if (C_blue_menu->GetValue())
    wxMessageBox(_T("blue menu box checked!"), _T("CheckBox!"));
  else
    wxMessageBox(_T("blue menu box unchecked!"), _T("CheckBox!"));
}

void Tabs::OnScalarBar(wxCommandEvent& event)
{
  if (C_scalar_bar->GetValue())
    wxMessageBox(_T("scalar bar box checked!"), _T("CheckBox!"));
  else
    wxMessageBox(_T("scalar bar box unchecked!"), _T("CheckBox!"));
}

void Tabs::OnRecord(wxCommandEvent& event)
{
  wxMessageBox(_T("Record button clicked!"), _T("Button!"));
}

void Tabs::OnExit(wxCommandEvent& event)
{
  wxMessageBox(_T("Exit button clicked!"), _T("Button!"));
}

void Tabs::CreateInitialPages()
{
    wxPanel *panel = (wxPanel *) NULL;

    // Create and add some panels to the notebook

    panel = CreateFirstPage();
    AddPage( panel, _T("Visualization"), true, 0 );

    panel = CreateSecondPage();
    AddPage( panel, _T("Streamlines"), false, 1);
}


MyFrame::MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size, long style)
    : wxFrame((wxWindow *) NULL, -1, title, pos, size, style)
{
    m_tabs = ( Tabs*) NULL;

    // create a dummy image list with a few icons
    wxSize imageSize(32, 32);

    m_imageList = new wxImageList( imageSize.GetWidth(), imageSize.GetHeight() );

    m_imageList->Add(wxArtProvider::GetIcon(wxART_INFORMATION, wxART_OTHER, imageSize));

    m_imageList->Add(wxArtProvider::GetIcon(wxART_QUESTION, wxART_OTHER, imageSize));

    m_imageList->Add(wxArtProvider::GetIcon(wxART_WARNING, wxART_OTHER, imageSize));

    m_imageList->Add(wxArtProvider::GetIcon(wxART_ERROR, wxART_OTHER, imageSize));

    m_tabs = new Tabs(this, -1);

    // Create the notebook's panels
    m_tabs->AssignImageList(m_imageList);
    m_tabs->CreateInitialPages();
    
    
    wxNotebookSizer *nbs = new wxNotebookSizer(m_tabs);
   
    m_sizerFrame = new wxBoxSizer(wxVERTICAL);
    m_sizerFrame->Add(nbs, 1, wxEXPAND | wxALL);
    m_sizerFrame->Layout();
    SetSizer(m_sizerFrame);

    SetAutoLayout(TRUE);

    m_sizerFrame->Fit(this);

}

MyFrame::~MyFrame()
{
}

