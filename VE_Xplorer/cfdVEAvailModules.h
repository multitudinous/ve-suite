#ifndef CFD_AVAIL_MODULES_H
#define CFD_AVAIL_MODULES_H
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>
//#include <vector>
//#include "wx/image.h"
//#include "wx/imaglist.h"
//#include "wx/treectrl.h"

#include "cfdVEPluginLoader.h"
//#include "Network.h"

//#include "icon1.xpm"
//#include "icon2.xpm"
//#include "icon3.xpm"
//#include "icon4.xpm"
//#include "icon5.xpm"


/*enum {
  
  Module_Desc,
  Module_Help,
  Module_Add,
  TREE_CTRL = 1000
};*/

class cfdVEAvail_Modules : public wxObject
{
  DECLARE_DYNAMIC_CLASS(cfdVEAvail_Modules);

 public:
  /*enum
    {
      TreeCtrlIcon_File,
      TreeCtrlIcon_FileSelected,
      TreeCtrlIcon_Folder,
      TreeCtrlIcon_FolderSelected,
      TreeCtrlIcon_FolderOpened
    };*/

  //Avail_Modules() {};
  //Avail_Modules(wxWindow *parent, const wxWindowID id, const wxPoint& pos, const wxSize& size,long style);

   cfdVEAvail_Modules( void );
   //~cfdVEAvail_Modules
  bool LoadModules(); //Load all the modules from the dlls 
  //void OnItemRightClick(wxTreeEvent& event);
  //void OnSelChanged(wxTreeEvent& event);
  //void ShowMenu(wxTreeItemId id, const wxPoint &pt);
  //void CreateImageList(int size=16);
  //void AddModule(REI_Plugin* plugin,wxClassInfo* clsi );
  //void ShowDesc(wxCommandEvent& event);
  //void ShowHelp(wxCommandEvent& event);
  //void Instantiate(wxTreeEvent& event);
  //void SetNetwork(Network *nw) { network = nw; };

 protected:
  
  //void getLeveledName(wxString name, vector<wxString> & lnames);


  //int m_imageSize;
  cfdVEPluginLoader* pl_loader;

  //wxTreeItemId rootId;
  //wxTreeItemId selection;
  //Network* network;

  //DECLARE_EVENT_TABLE();
};

/*class ReiTreeItemData : public wxTreeItemData
{
 public:
  REI_Plugin* plugin;
  wxClassInfo* pl_clsi;

  ReiTreeItemData(REI_Plugin* pl,  wxClassInfo* clsi  ) { plugin = pl; pl_clsi = clsi; };

};*/

#endif
