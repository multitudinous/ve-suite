#ifndef NETWORK_H
#define NETWORK_H
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>
#include <vector>
#include <map>
#include <iostream>
#include "PluginLoader.h"
#include "moduleC.h"
#include "GlobalParamDialog.h"

class Geometry;

using namespace std;

enum 
{
   ADD_TAG,
   ADD_LINK_CON,
   EDIT_TAG,
   DEL_TAG,
   DEL_LINK,
   DEL_LINK_CON,
   DEL_MOD,
   SHOW_LINK_CONT,
   SHOW_RESULT,
   SHOW_DESC,
   PARAVIEW,
   SHOW_FINANCIAL, /* EPRI TAG */
   GEOMETRY
};

typedef struct {
  unsigned long Fr_mod;
  unsigned long To_mod;
  int Fr_port;
  int To_port;

  vector<wxPoint> cons; //connectors
  POLY poly; //Poly is the current poly on the canvas
} LINK;
 
typedef struct {
  REI_Plugin * pl_mod;
  POLY poly; //Poly is the current poly on the canvas
  vector<LINK*> links; //links connected with me
  string cls_name;
} MODULE;

typedef struct {
  wxPoint cons[2]; //2 connectors for tag, end and middle
  wxString text;
  wxRect box;
  POLY poly; //Poly is the current poly on the canvas
} TAG;

class Network : public wxScrolledWindow
{
  DECLARE_DYNAMIC_CLASS(Network)
 public:
  Network() {};
  Network(wxWindow* parent, int id);
  Network::~Network();
  double m_xUserScale, m_yUserScale; //Zoom Factor
  int nPixX, nPixY; 
  int nUnitX, nUnitY; //Unit of the scrolled window
  Body::Executive_var exec; //put this reference here, so ther frame work can still access it YANG
 
  wxMutex s_mutexProtect;

  GlobalParamDialog * globalparam_dlg;

  map<int, MODULE> modules; //The list of modules;
  
  bool paraview;

  void ReDrawAll();
  
  //Event Handlers
  void OnPaint(wxPaintEvent &event);
  void OnMouseMove(wxMouseEvent &event);
  void OnMLeftDown(wxMouseEvent &event);
  void OnMLeftUp(wxMouseEvent &event);
  void OnDClick(wxMouseEvent &event);
  void OnMRightDown(wxMouseEvent &event);
  void OnAddTag(wxCommandEvent &event);
  void OnAddLinkCon(wxCommandEvent &event);
  void OnEditTag(wxCommandEvent &event);
  void OnDelTag(wxCommandEvent &event);
  void OnDelLink(wxCommandEvent &event);
  void OnDelLinkCon(wxCommandEvent &event);
  void OnDelMod(wxCommandEvent &event);
  void OnShowLinkContent(wxCommandEvent &event);
  void OnShowResult(wxCommandEvent &event);
  void OnParaView(wxCommandEvent &event);
  void OnShowDesc(wxCommandEvent &event);
  void OnGeometry(wxCommandEvent &event);

  // EPRI TAG
  void OnShowFinancial(wxCommandEvent &event);

  //Add to network fuctions
  void AddtoNetwork(REI_Plugin *new_mod, string cls_name);
  void AddTag(int x, int y, wxString text);
  
  //Save and Load the network
  void Save(wxString filename);
  void SaveS(std::string &network_pack);            // save the network to a string
  void Load(wxString filename);
  void LoadS(const char* inputs); //load the network from a string
  void Network::New();
 protected:
  
  //Draw functions
  void DrawPorts(REI_Plugin * cur_module, bool flag);
  void DrawPorti(REI_Plugin * cur_module, int index, bool flag);
  void DrawLinkCon(LINK l, bool flag);
  void DrawTagCon(TAG t, bool flag);
  void DrawLink(LINK *l, bool flag);
  void DrawTag(TAG *t, bool flag);
  void ReDraw(wxDC &dc);

  //Selection functions
  int SelectMod(int x, int y);
  void UnSelectMod(wxDC& dc);
  int  SelectLink(int x, int y);
  void UnSelectLink(wxDC& dc);
  int SelectTag(int x, int y);
  void UnSelectTag(wxDC& dc);

  //Move and drop functions
  void MoveModule(int x, int y, int mod, wxDC &dc);
  void DropModule(int x, int y, int mod, wxDC &dc);
  void MoveLinkCon(int x, int y, int ln, int ln_con, wxDC& dc);
  void DropLinkCon(int x, int y, int ln, int ln_con, wxDC& dc);
  void MoveTagCon(int x, int y, int t, int t_con, wxDC &dc);
  void DropTagCon(int x, int y, int t, int t_con, wxDC &dc);
  void MoveTag(int x, int y, int t, wxDC &dc);
  void DropTag(int x, int y, int t, wxDC &dc);
  void TryLink(int x, int y, int mod, int pt, wxDC &dc, bool flag);
  void DropLink(int x, int y, int mod, int pt, wxDC &dc, bool flag);

  //Math functions for the relationship of points and polygons
  int ccw(wxPoint pt1, wxPoint pt2, wxPoint pt3);
  int intersect(POLY l1, POLY l2);
  int inside(wxPoint pt, POLY poly);
  double computenorm (wxPoint pt1, wxPoint pt2);
  double nearpnt(wxPoint pt, POLY poly, POLY &near);
  void TransPoly(POLY oldpoly, int x, int y, POLY &newpoly);

  //Misc functions
  void CleanRect(wxRect box, wxDC& dc); // for wipeout a rectangular area
  wxPoint GetFreePos(wxRect bbox); // for finding a free start location for a new module
  POLY Network::CalcLinkPoly(LINK l); // calculate the bounding polygon of a link
  POLY Network::CalcTagPoly(TAG t); // calculate the bounding polygon of a tag

 protected:
  //Three main list of network objs
 
  vector<LINK*> links; //The list of links between the nodes of moduls.
  vector<TAG> tags; //The list of text tags  

 
  
  int m_selMod; // selected module
  int m_selFrPort; // selected From port
  int m_selToPort; // selected To port;
  int m_selLink; //selected Link
  int m_selLinkCon; //selected Link Connector
  int m_selTag; //selected Tag
  int m_selTagCon; //selected Tag Connector

  wxPoint relative_pt; // the relative point of the polygon, used by the move module function
  wxPoint tag_rpt; // the relative point of the tag
  
  void Pack(vector<Interface> & UIs);
  void UnPack(vector<Interface> & UIs);
 private:
  bool moving;
  vector<wxRect> sbboxes; //start up bounding box; used by GetFreePos to calc start module location
  int xold, yold; //The old location of the mouse position, used by the TryLink to wipe the old tried link route
  wxPoint action_point; //The mouse position when the right button clicked, used by menu event handlers

  DECLARE_EVENT_TABLE();
};

#endif
