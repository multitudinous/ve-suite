#include "cfdVEAvailModules.h"
#include <fstream>
#include <sstream>
//#include "StringParse.h"

/*BEGIN_EVENT_TABLE(Avail_Modules, wxTreeCtrl)
  EVT_TREE_ITEM_RIGHT_CLICK(TREE_CTRL, Avail_Modules::OnItemRightClick)
  EVT_TREE_SEL_CHANGED(TREE_CTRL, Avail_Modules::OnSelChanged)
  EVT_MENU(Module_Desc, Avail_Modules::ShowDesc)
  EVT_MENU(Module_Help, Avail_Modules::ShowHelp)
  EVT_TREE_ITEM_ACTIVATED(TREE_CTRL, Avail_Modules::Instantiate)
END_EVENT_TABLE()*/
//using namespace std;
   
//IMPLEMENT_DYNAMIC_CLASS(Avail_Modules, wxTreeCtrl)
//IMPLEMENT_DYNAMIC_CLASS(cfdVEAvail_Modules, wxObject)

/*Avail_Modules::Avail_Modules(wxWindow *parent, const wxWindowID id, const wxPoint& pos, const wxSize& size,long style)
  : wxTreeCtrl(parent, id, pos, size, style)
{
  
  int image1 = TreeCtrlIcon_Folder;
  int image2 = TreeCtrlIcon_FolderSelected;
  //CreateImageList();
  rootId = AddRoot(wxT("Available Modules"),
				image1, image2,
				NULL);
  SetItemImage(rootId, TreeCtrlIcon_FolderOpened, wxTreeItemIcon_Expanded);
  SetItemFont(rootId, *wxITALIC_FONT);
  pl_loader = new PluginLoader();
  LoadModules();
}*/

cfdVEAvail_Modules::cfdVEAvail_Modules( void )
{
  pl_loader = new cfdVEPluginLoader();
  LoadModules();
}

cfdVEAvail_Modules::~cfdVEAvail_Modules( void )
{
  delete pl_loader;
}

bool cfdVEAvail_Modules::LoadModules()
{
   //int i;
   char* path = getenv("PWD");
   char* modelPath = "/Plugins";
   char* file = new char[100];
   
   strcpy( file, path );
   strcat( file, modelPath );

   wxString wxPath = file;
   pl_loader->LoadPlugins( wxPath );
  
   //for (i=0; i<pl_loader->plugins.size(); i++)
   //AddModule(pl_loader->plugins[i], pl_loader->plugin_cls[i]);
   delete [] file;

   return true;
}

cfdVEPluginLoader* cfdVEAvail_Modules::GetLoader( void )
{
   return pl_loader;
}

/*void Avail_Modules::AddModule(REI_Plugin* plugin, wxClassInfo* clsi)
{
  vector<wxString> lnames;
  wxTreeItemIdValue cookie;
  wxString plname = plugin->GetName();
  wxTreeItemId id, lastid;
  int image1, image2, image3, image4;
  int i, lsize;

  getLeveledName(plname, lnames);
  image1 = TreeCtrlIcon_Folder;
  image2 = TreeCtrlIcon_FolderSelected;
  image3 = TreeCtrlIcon_File;
  image4 = TreeCtrlIcon_FileSelected;

  id = rootId;
  lsize = lnames.size();
  for (i=0; i<(lsize-1); i++)
    {
      lastid=id;
      id = GetFirstChild(id, cookie);
      while (1)
	{
	  
	  if (id<=0)
	    {
	      id=AppendItem(lastid,lnames[i],
			 image1, image2,
			 NULL);
	      SetItemImage(id, TreeCtrlIcon_FolderOpened, wxTreeItemIcon_Expanded);
	      SetItemFont(id, *wxITALIC_FONT);
	      break;
	    }
	  	  
	  if (GetItemText(id)==lnames[i])
	    break;
	  
	  id= GetNextChild(lastid, cookie);
	}
    }
   
   id = AppendItem(id, lnames[i], image3, image4,  new ReiTreeItemData(plugin, clsi));
   SetItemBold(id);
  
}*/

/*void Avail_Modules::getLeveledName(wxString name, vector<wxString> & lnames)
{
  char * s;
  int len;

  len = name.Len();
  s = new char[len+1];

  lnames.clear();
  strcpy(s, name.c_str());
  get_tokens(s, lnames, "_");
  delete [] s;
  
}*/


/*void Avail_Modules::OnItemRightClick(wxTreeEvent& event)
{
  ReiTreeItemData* item_data;

  selection = GetSelection();
  if (selection<=0)
    return;

  item_data = (ReiTreeItemData*) GetItemData(selection);
  if (item_data==NULL)
    return;

  ShowMenu(selection, event.GetPoint());
}*/

/*void Avail_Modules::Instantiate(wxTreeEvent& event) //Double click
{
  ReiTreeItemData* item_data;
  wxClassInfo* info;
  REI_Plugin *object;

  selection = GetSelection();
  if (selection<=0)
    return;

  item_data = (ReiTreeItemData*) GetItemData(selection);
  if (item_data==NULL)
    return;

  info = item_data->pl_clsi;
  if (info)
    {
      object = (REI_Plugin *) info->CreateObject();
      network->AddtoNetwork(object, info->GetClassName());
      //      cout<<"a moduel size : "<<network->modules.size()<<endl;
      //(network->modules).push_back(object);
      //wxString title, desc;
      
      //char s[50];
      //sprintf(s, "%ul", object);
      //title << wxT("Description for ") << GetItemText(selection);
      //title<<s;
      // desc = object->GetDesc();
      
      // wxMessageDialog(this, desc, title).ShowModal();
    }
  
}*/

/*void Avail_Modules::ShowMenu(wxTreeItemId id, const wxPoint& pt)
{
    wxString title;
    if ( id.IsOk() )
    {
        title << wxT("Menu for ") << GetItemText(id);
    }
    else
    {
        title = wxT("Menu for no particular item");
    }

    wxMenu menu(title);
    menu.Append(Module_Desc, wxT("&Description"));
    menu.Append(Module_Help, wxT("&Help"));

    PopupMenu(&menu, pt);
}*/


/*void Avail_Modules::CreateImageList(int size)
{
    if ( size == -1 )
    {
        SetImageList(NULL);
        return;
    }
    if ( size == 0 )
        size = m_imageSize;
    else
        m_imageSize = size;

    // Make an image list containing small icons
    wxImageList *images = new wxImageList(size, size, TRUE);

    // should correspond to TreeCtrlIcon_xxx enum
    wxBusyCursor wait;
    wxIcon icons[5];
    icons[0] = wxIcon(icon1_xpm);
    icons[1] = wxIcon(icon2_xpm);
    icons[2] = wxIcon(icon3_xpm);
    icons[3] = wxIcon(icon4_xpm);
    icons[4] = wxIcon(icon5_xpm);

    int sizeOrig = icons[0].GetWidth();
    for ( size_t i = 0; i < WXSIZEOF(icons); i++ )
    {
        if ( size == sizeOrig )
        {
            images->Add(icons[i]);
        }
        else
        {
            images->Add(wxBitmap(wxBitmap(icons[i]).ConvertToImage().Rescale(size, size)));
        }
    }

    AssignImageList(images);
}*/

/*void Avail_Modules::OnSelChanged(wxTreeEvent& event)
{
}*/



/*void Avail_Modules::ShowDesc(wxCommandEvent& event)
{
  ReiTreeItemData* item_data;
  REI_Plugin* pl;
  wxString desc;
  wxString title;
 
  title << wxT("Description for ") << GetItemText(selection);
  
  if (selection<=0)
    return;
  
  item_data = (ReiTreeItemData*) GetItemData(selection);
  if (item_data==NULL)
    return;

  pl = item_data->plugin;
  desc = pl->GetDesc();
  
  wxMessageDialog(this, desc, title).ShowModal();
   
}

void Avail_Modules::ShowHelp(wxCommandEvent& event)
{
}*/
