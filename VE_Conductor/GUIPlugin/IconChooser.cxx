#include "IconChooser.h"
#include <wx/dir.h>
#include <wx/image.h>

BEGIN_EVENT_TABLE(IconChooser,wxDialog)	
	EVT_CLOSE(IconChooser::OnClose)
	EVT_BUTTON(1002,IconChooser::okButtonClick)
	EVT_BUTTON(1003,IconChooser::cancelButtonClick)
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////////////////////
IconChooser::IconChooser(wxWindow *parent, std::string path,wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	directory = wxString(path.c_str(), wxConvUTF8);
	CreateGUIControls();
}
////////////////////////////////////////////////////////////////////////////////
IconChooser::~IconChooser()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::CreateGUIControls()
{
	wxPanel * WxPanel = new wxPanel(this, 1000, wxPoint(0,0), wxSize(640,480));
	wxNotebook * WxNotebook = new wxNotebook(WxPanel, 1001, wxPoint(0,0),wxSize(617,460));
	WxEdit = new wxTextCtrl(WxPanel, 1002, wxT(""), wxPoint(40, 463), wxSize(400,21), 0, wxDefaultValidator, wxT(""));
	WxEdit->SetEditable(false);
	okButton = new wxButton(WxPanel, 1002, wxT("OK"), wxPoint(450, 463));
	cancelButton = new wxButton(WxPanel, 1003, wxT("Cancel"), wxPoint(535, 463));
	//WxChoice = new wxChoice(WxPanel, 1003, wxPoint(220,3), wxSize(200,21), componentList, 0, wxDefaultValidator, wxT("Components"));
	//WxChoice->SetSelection(-1);

   {
      int buttonCount = 4000;
      std::vector< wxImage > defaultIcons;
      defaultIcons.push_back( wxImage( contour_xpm ) );
      iconPaths[buttonCount] = "contour.xpm";
      //defaultIcons.push_back( wxImage( cad_tree_selected_xpm ) );
      //iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );
      //defaultIcons.push_back( wxImage( cad_tree_unselected_xpm ) );
      //iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );
      //defaultIcons.push_back( wxImage( cspline_xpm ) );
      //iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );
      defaultIcons.push_back( wxImage( isosurface_xpm ) );
      iconPaths[buttonCount+1] = "isosurface.xpm";
      defaultIcons.push_back( wxImage( ROItb_xpm ) );
      iconPaths[buttonCount+2] = "isosurface.xpm";
      //defaultIcons.push_back( wxImage( square_xpm ) );
      //iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );
      defaultIcons.push_back( wxImage( streamlines_xpm ) );
      iconPaths[buttonCount+3] = "streamlines.xpm";
      defaultIcons.push_back( wxImage( vector_xpm ) );
      iconPaths[buttonCount+4] = "vector.xpm";
      defaultIcons.push_back( wxImage( vectortb_xpm ) );
      iconPaths[buttonCount+5] = "vectortb.xpm";

      wxPanel* WxNoteBookPage = new wxPanel(WxNotebook);
      WxNotebook->AddPage(WxNoteBookPage, wxString( _("Default Icons") ) );
      
      int hCount = 0;
      int vCount = 0;
      int xLoc = 0;
      int yLoc = 0;

      for ( size_t i = 0; i < defaultIcons.size(); ++i )
      {
         //place the button and its label on the current page
         xLoc = 60 * hCount;
         yLoc = 80 * vCount;
         //yLoc = 95 * vCount;
         wxBitmapButton * tempButton = new wxBitmapButton(WxNoteBookPage, buttonCount, defaultIcons.at( i ), wxPoint(xLoc, yLoc));
         tempButton->SetToolTip( _("Default Icon") );
         Connect(buttonCount, wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler(IconChooser::WxButtonClick));
         //wxStaticText * iconLabel = new wxStaticText(WxNoteBookPage, 9999, filename, wxPoint(xLoc, yLoc + 80), wxDefaultSize, 0, filename);
         buttonCount++;
         hCount++;
         //set how many buttons can be placed horizonatally
         //currently 10 buttons
         if(hCount == 10)
         {
            hCount = 0;
            vCount ++;
         }
      }
   }

   //Parse the default directory structure
   //wxString directory( _("F:/ASPENV21/2DIcons") );
   wxString dirname;
   wxDir parentDir (directory);
   bool isParentTrue = parentDir.GetFirst(&dirname);
   int buttonCount = 2000;
   while(isParentTrue)
   {
      //do not include the .svn folders
      if ( dirname.compare(wxT(".svn")) != 0)
      {
         wxPanel * WxNoteBookPage = new wxPanel(WxNotebook);
         WxNotebook->AddPage(WxNoteBookPage, dirname);

         wxString filename;
         wxDir childDir (directory+ wxT("/")+dirname);
         bool isChildTrue = childDir.GetFirst(&filename);
         int hCount = 0;
         int vCount = 0;
         int xLoc = 0;
         int yLoc = 0;
         while(isChildTrue)
         {
            //do not include the .svn folders
            if ( filename.compare(wxT(".svn")) != 0 )
            {
               //construct iconPath and place it in the map along with its event id
               filename = filename.RemoveLast(4);
               wxString iconPath = dirname+ wxString(_("/"))+filename;
               iconPaths[buttonCount] = ConvertUnicode( iconPath.c_str() );

               //create the image for the button and scale it
               wxInitAllImageHandlers();
               wxImage jpeg (directory + wxT("/") + iconPath + wxT(".jpg"));
               jpeg = jpeg.Scale(50, 70);

               //place the button and its label on the current page
               xLoc = 60 * hCount;
               yLoc = 80 * vCount;
               //yLoc = 95 * vCount;
               wxBitmapButton * tempButton = new wxBitmapButton(WxNoteBookPage, buttonCount, jpeg, wxPoint(xLoc, yLoc));
               tempButton->SetToolTip(filename);
               Connect(buttonCount, wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler(IconChooser::WxButtonClick));
               //wxStaticText * iconLabel = new wxStaticText(WxNoteBookPage, 9999, filename, wxPoint(xLoc, yLoc + 80), wxDefaultSize, 0, filename);

               buttonCount++;
               hCount++;

               //set how many buttons can be placed horizonatally
               //currently 10 buttons
               if ( hCount == 10 )
               {
                  hCount = 0;
                  vCount ++;
               }
            }
            isChildTrue = childDir.GetNext(&filename);
         }
      }
      isParentTrue = parentDir.GetNext(&dirname);
   }

	SetTitle(wxT("VE Icon Chooser"));
	SetIcon(wxNullIcon);
	SetSize(8,8,623,530);
	Center();
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::OnClose(wxCloseEvent& event)
{
	Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::WxButtonClick(wxCommandEvent& event)
{
    //wxString id;
    //id.Printf("%d", event.GetId());
	//WxEdit->SetValue(id);
	WxEdit->SetValue( wxString( iconPaths[event.GetId()].c_str(), wxConvUTF8 ) );
	//thePlugin->SetImageIcon(iconPaths[event.GetId()]);
}
////////////////////////////////////////////////////////////////////////////////
//void IconChooser::AppendList(const char * input)
//{
//	WxChoice->Append(wxString(input,wxConvUTF8));
//}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::SetPlugin( REI_Plugin * plugin)
{
	thePlugin = plugin;
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::okButtonClick(wxCommandEvent& event)
{
	thePlugin->SetImageIcon(WxEdit->GetValue().c_str());
	Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void IconChooser::cancelButtonClick(wxCommandEvent& event)
{
	Destroy();
}
////////////////////////////////////////////////////////////////////////////////