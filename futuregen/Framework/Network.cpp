
#include "Network.h"
#include "PortDialog.h"
#include <math.h>
#include "package.h"
#include "paraThread.h"

IMPLEMENT_DYNAMIC_CLASS(Network, wxScrolledWindow);
BEGIN_EVENT_TABLE(Network, wxScrolledWindow)
  EVT_PAINT(Network::OnPaint)
  EVT_MOTION(Network::OnMouseMove)
  EVT_LEFT_DOWN(Network::OnMLeftDown)
  EVT_LEFT_UP(Network::OnMLeftUp)
  EVT_LEFT_DCLICK(Network::OnDClick)
  EVT_RIGHT_DOWN(Network::OnMRightDown)
  EVT_MENU(ADD_TAG, Network::OnAddTag)
  EVT_MENU(ADD_LINK_CON, Network::OnAddLinkCon)
  EVT_MENU(EDIT_TAG, Network::OnEditTag)
  EVT_MENU(DEL_TAG, Network::OnDelTag)
  EVT_MENU(DEL_LINK, Network::OnDelLink)
  EVT_MENU(DEL_LINK_CON, Network::OnDelLinkCon)
  EVT_MENU(DEL_MOD, Network::OnDelMod)
  EVT_MENU(SHOW_LINK_CONT, Network::OnShowLinkContent)
  EVT_MENU(SHOW_RESULT, Network::OnShowResult)
  EVT_MENU(PARAVIEW, Network::OnParaView)
  EVT_MENU(SHOW_DESC, Network::OnShowDesc)
END_EVENT_TABLE()

Network::Network(wxWindow* parent, int id)
  :wxScrolledWindow(parent, id, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxNO_FULL_REPAINT_ON_RESIZE)
{
  modules.clear();
  links.clear();
  m_xUserScale=1;
  m_yUserScale=1;
  nUnitX=100;
  nUnitY=240;
  nPixX = 10;
  nPixY = 10;
  SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
  m_selMod = -1;
  m_selFrPort = -1; 
  m_selToPort = -1; 
  m_selLink = -1; 
  m_selLinkCon = -1; 
  m_selTag = -1; 
  m_selTagCon = -1; 
  xold = yold =0;
  moving = false;
  paraview = false;
  globalparam_dlg = new GlobalParamDialog(NULL, -1);;
  SetBackgroundColour(*wxWHITE);
}

Network::~Network()
{
  int i;
  map<int, MODULE>::iterator iter;
  for (i=0; i< links.size(); i++)
    delete links[i];
  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i=iter->first;
      delete modules[i].pl_mod;
    }
  delete globalparam_dlg;
}
/////////////////////////////////////////////
///////// Event Handlers ////////////////////
/////////////////////////////////////////////

void Network::OnPaint(wxPaintEvent &event)
{
  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);
  int i;
  wxPaintDC dc(this);
  PrepareDC(dc);
  
  dc.SetUserScale( m_xUserScale, m_yUserScale );
 // dc.Clear();
  
  ReDraw(dc); 

  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
}

/////////////////////////////////////////////////////////////
void Network::OnMLeftDown(wxMouseEvent &event)
{
  wxRect bbox;
  wxPoint pos, temp;
  map<int, MODULE>::iterator iter;
  int i ;
  POLY ports;
  int num;
 	
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );

  wxPoint evtpos = event.GetLogicalPosition(dc);
 
  //First, check if any module is selected
  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i=iter->first;
      temp.x = evtpos.x;
      temp.y = evtpos.y;
      if (inside(temp, modules[i].poly))
	{
	  m_selMod = i;
	  bbox = modules[i].pl_mod->GetBBox();
	  relative_pt.x = temp.x - bbox.x;
	  relative_pt.y = temp.y - bbox.y;
	  break;
	}
    }

  //Second, check if selected module's Iports is selected
  if (m_selMod>=0)
    {
      bbox = modules[m_selMod].pl_mod->GetBBox();
      pos.x = bbox.x;
      pos.y = bbox.y;
      
      temp.x = evtpos.x - pos.x;
      temp.y = evtpos.y - pos.y;

      num = modules[m_selMod].pl_mod->GetNumIports();
      ports.resize(num);
      modules[m_selMod].pl_mod->GetIPorts(ports);
      for (i=0; i<ports.size(); i++)
	if (computenorm(temp, ports[i])<=3)
	  {
	    m_selFrPort = i;
	    break;
	  }
  
	  num = modules[m_selMod].pl_mod->GetNumOports();
      ports.resize(num);
	  modules[m_selMod].pl_mod->GetOPorts(ports);
      for (i=0; i<ports.size(); i++)
	if (computenorm(temp, ports[i])<=3)
	  {
	    m_selToPort = i;
	    break;
	  }
    }
  //Third, check if any link connector is selected

  if (m_selLink>=0)
    {
      for (i=0; i<links[m_selLink]->cons.size(); i++)
	if (computenorm(evtpos, links[m_selLink]->cons[i])<=3)
	  {
	    m_selLinkCon=i;
	    break;
	  }
    }

  //Forth, check if any tag is selected
  for (i=0; i<tags.size(); i++)
    {
      if (tags[i].box.Inside(evtpos))
	{
	  m_selTag=i;
	  tag_rpt.x = evtpos.x - tags[i].box.x;
	  tag_rpt.y = evtpos.y - tags[i].box.y;
	  break;
	}
    }

  //At last, check if any tag connector is selected
  for (i=0; i<tags.size(); i++)
    {
      if (computenorm(evtpos, tags[i].cons[0])<=3)
	{
	  m_selTag=i;
	  m_selTagCon=0;
	  break;
	}
      if (computenorm(evtpos, tags[i].cons[1])<=3)
	{
	  m_selTag=i;
	  m_selTagCon=1;
	  break;
	}
    }
}

////////////////////////////////////////////////////////////////////
void Network::OnMouseMove(wxMouseEvent &event)
{
  long x, y;
  int i;
  
  wxPoint evtpos;

  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );
  
  evtpos = event.GetLogicalPosition(dc);
  
  x = evtpos.x;
  y = evtpos.y;

  if (!event.Dragging())
    {
	  if (moving)
	  {
		OnMLeftUp(event);
		Refresh();
	  }
	  
      if (m_selMod>=0 && SelectMod(x, y)<0 ) 
	  		UnSelectMod(dc); //Unselect only get called by state changed from something selected state to nothing selected state
	  else if (m_selLink>=0 && SelectLink(x, y )<0)
	UnSelectLink(dc);
      else if (m_selTag>=0 && SelectTag(x, y)<0)
	UnSelectTag(dc);
      else
	{
	  // To avoid the shortcut evaluation
	  SelectMod(x, y);
	  SelectLink(x, y );
	  SelectTag(x, y); 
	  //Check if link con is selected
	  m_selLinkCon = -1;
	  if (m_selLink>=0)
	    {
	      for (i=0; i<links[m_selLink]->cons.size(); i++)
		if (computenorm(evtpos, links[m_selLink]->cons[i])<=3)
		  {
		    m_selLinkCon=i;
		    break;
		  }
	    }
	 
	  //Check if tag con is selected
	  m_selTagCon = -1;
	  if (m_selTag>=0)
	    {
	      if (computenorm(evtpos, tags[m_selTag].cons[0])<=3)
		m_selTagCon=0;
	      if (computenorm(evtpos, tags[m_selTag].cons[1])<=3)
		m_selTagCon=1;
	    }
	  
	}
    }
  else //dragging
    {
	  moving = true; 
      if (m_selLinkCon>=0 && m_selLink>=0)
	MoveLinkCon(x, y, m_selLink, m_selLinkCon, dc);
      else if (m_selTag>=0 && m_selTagCon<0)
	MoveTag(x, y, m_selTag, dc);
      else if (m_selTag>=0 && m_selTagCon>=0)
	MoveTagCon(x, y, m_selTag, m_selTagCon, dc);
      else if (m_selMod>=0 && m_selFrPort>=0)
	TryLink(x, y, m_selMod, m_selFrPort, dc, true);
      else if (m_selMod>=0 && m_selToPort>=0)
	TryLink(x, y, m_selMod, m_selToPort, dc, false);
      else if (m_selMod>=0 && m_selFrPort<0 && m_selToPort<0)
	MoveModule(x, y, m_selMod, dc);
    }
}

/////////////////////////////////////////////////////////////////////
void Network::OnMLeftUp(wxMouseEvent &event)
{
  long x, y;
  wxPoint evtpos;

  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );

  evtpos = event.GetPosition();
  x = dc.DeviceToLogicalX(evtpos.x );
  y = dc.DeviceToLogicalY(evtpos.y );
  
  if (m_selLinkCon>=0 && m_selLink>=0)
    {
      DropLinkCon(x, y, m_selLink, m_selLinkCon, dc);
      m_selLinkCon = -1;
      m_selLink=-1;
    }
  else if (m_selTag>=0 && m_selTagCon<0)
    {
      DropTag(x, y, m_selTag, dc);
      m_selTag=-1;
    }
  else if (m_selTag>=0 && m_selTagCon>=0)
    {
      DropTagCon(x, y, m_selTag, m_selTagCon, dc);
      m_selTag=-1;
      m_selTagCon=-1;
    }
  else if (m_selMod>=0 && m_selFrPort>=0)
    {
      DropLink(x, y, m_selMod, m_selFrPort, dc, true);
      m_selMod = -1;
      m_selFrPort = -1;
    }
  else if (m_selMod>=0 && m_selToPort>=0)
    {
      DropLink(x, y, m_selMod, m_selToPort, dc, false);
      m_selMod = -1;
      m_selToPort = -1;
    }
  else if (m_selMod>=0 && m_selFrPort<0 && m_selToPort<0)
    {
      DropModule(x, y, m_selMod, dc);
      m_selMod = -1;
    }
	moving = false;
}

////////////////////////////////////////////////////////////////
void Network::OnDClick(wxMouseEvent &event)
{
  wxPoint evtpos;
  UIDialog * hello;
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );
  
  evtpos = event.GetLogicalPosition(dc);
  
  SelectMod(evtpos.x, evtpos.y);

  if (m_selMod >= 0)
    {
      hello = modules[m_selMod].pl_mod->UI(NULL);
      if (hello!=NULL)
	hello->Show();
      m_selMod = -1;
    }
}

///////////////////////////////////////////////////////////////
void Network::OnMRightDown(wxMouseEvent &event)
{
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );

  wxMenu pop_menu(_T("Action"));
    
  pop_menu.Append(ADD_TAG, _T("Add Tag")); //This will always be enable

  pop_menu.Append(ADD_LINK_CON, _T("Add Link Connector"));
  pop_menu.Append(EDIT_TAG, _T("Edit Tag"));
  pop_menu.Append(DEL_LINK_CON, _T("Delete Link Connector"));
  pop_menu.Append(DEL_LINK, _T("Delete Link"));
  pop_menu.Append(DEL_TAG, _T("Delete Tag"));
  pop_menu.Append(DEL_MOD, "Del Module");

  pop_menu.Append(SHOW_DESC, "Show Module Description");	
  pop_menu.Append(SHOW_RESULT, "Show Module Result");
  pop_menu.Append(PARAVIEW, "ParaView 3D Result");
  
  pop_menu.Append(SHOW_LINK_CONT, "Show Link Content");

  pop_menu.Enable(ADD_LINK_CON, false);
  pop_menu.Enable(EDIT_TAG, false);
  pop_menu.Enable(DEL_LINK_CON, false);
  pop_menu.Enable(DEL_LINK, false);
  pop_menu.Enable(DEL_TAG, false);
  pop_menu.Enable(DEL_MOD, false);
  pop_menu.Enable(SHOW_RESULT, false);
  pop_menu.Enable(PARAVIEW, false);

  pop_menu.Enable(SHOW_LINK_CONT, false);

  if (m_selLink>=0)
    {
      pop_menu.Enable(DEL_LINK, true);
      pop_menu.Enable(SHOW_LINK_CONT, true);
      if (m_selLinkCon>=0) 
	pop_menu.Enable(DEL_LINK_CON, true);
      else
	pop_menu.Enable(ADD_LINK_CON, true);
    };

  if (m_selTag>=0 )
    {
      pop_menu.Enable(EDIT_TAG, true);
      pop_menu.Enable(DEL_TAG, true);
    };
  
  if (m_selMod>=0)
    {
      pop_menu.Enable(DEL_MOD, true);
      pop_menu.Enable(SHOW_RESULT, true);
      if (modules[m_selMod].pl_mod->Has3Ddata())
	pop_menu.Enable(PARAVIEW, true);
    };
    
  action_point = event.GetLogicalPosition(dc);
  PopupMenu(&pop_menu, event.GetPosition());
  
  
  m_selMod = -1;
  m_selFrPort = -1; 
  m_selToPort = -1; 
  m_selLink = -1; 
  m_selLinkCon = -1; 
  m_selTag = -1; 
  m_selTagCon = -1; 
  xold = yold =0;
}

//////// Menu event handlers ////////////////////////

void Network::OnAddTag(wxCommandEvent &event)
{
  wxTextEntryDialog dialog(this,_T("Tag Editor"), _T("Please enter the text for the tag : "),_T("this is a tag"), wxOK);

  if (dialog.ShowModal() == wxID_OK)
    AddTag(action_point.x, action_point.y, dialog.GetValue());
}

/////////////////////////////////////////////////////
void Network::OnAddLinkCon(wxCommandEvent &event)
{

  POLY linkline, temp, Near;
  wxRect bbox;
  std::vector<wxPoint> ports;  
  int n, i, j;
  int num;
  
  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);;
  DrawLink(links[m_selLink], false);

  n = links[m_selLink]->cons.size()+2;
  linkline.resize(n);

  bbox = modules[links[m_selLink]->Fr_mod].pl_mod->GetBBox();
  
  num = modules[links[m_selLink]->Fr_mod].pl_mod->GetNumOports();
  ports.resize(num);
  modules[links[m_selLink]->Fr_mod].pl_mod->GetOPorts(ports);

  linkline[0].x = bbox.x+ports[links[m_selLink]->Fr_port].x;
  linkline[0].y = bbox.y+ports[links[m_selLink]->Fr_port].y;

  for (i=0; i<links[m_selLink]->cons.size(); i++)
    linkline[i+1]=links[m_selLink]->cons[i];

  bbox = modules[links[m_selLink]->To_mod].pl_mod->GetBBox();

  num = modules[links[m_selLink]->To_mod].pl_mod->GetNumIports();	
  ports.resize(num);
  modules[links[m_selLink]->To_mod].pl_mod->GetIPorts(ports);
  int tempi;
  tempi = links[m_selLink]->To_port;
  linkline[n-1].x = bbox.x+ports[tempi].x;
  tempi = links[m_selLink]->To_port;
  linkline[n-1].y = bbox.y+ports[tempi].y;
  
  nearpnt(action_point, linkline, Near);

  for (i=0; i<linkline.size()-1; i++)
    if((linkline[i].x <= Near[0].x && linkline[i+1].x>=Near[0].x)
	||(linkline[i].x>=Near[0].x && linkline[i+1].x<=Near[0].x))
      break;

  for (j=1; j<linkline.size()-1; j++)
    {
      if (j-1==i)
	temp.push_back(Near[0]);
      temp.push_back(linkline[j]);
    }

  if (j==1 && i==0) //Between the first link con and the port, and no cons yet
    temp.push_back(Near[0]);

   if (j==linkline.size()-1 && i==(j-1) && i!=0) //between the port and the las con
    temp.push_back(Near[0]);
  
  links[m_selLink]->cons=temp;
  DrawLinkCon(*links[m_selLink], true);
  m_selLink = -1;
  m_selLinkCon = -1;
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
    
}

/////////////////////////////////////////////////////
void Network::OnEditTag(wxCommandEvent &event)
{
  wxClientDC dc(this);
  PrepareDC(dc);
  
  dc.SetUserScale( m_xUserScale, m_yUserScale );
  int w, h;

  wxString tag_text;

  tag_text=tags[m_selTag].text;
  wxTextEntryDialog dialog(this,_T("Tag Editor"), _T("Please enter the text for the tag : "),tag_text, wxOK);
  
  if (dialog.ShowModal() == wxID_OK)
    tag_text=dialog.GetValue();

  
  dc.GetTextExtent(tag_text, &w, &h);
  
  tags[m_selTag].text = tag_text;
  tags[m_selTag].box.width = w;
  tags[m_selTag].box.height = h;
  tags[m_selTag].poly = CalcTagPoly(tags[m_selTag]);
  //  Refresh(false);
  m_selTag = -1;
}

/////////////////////////////////////////////////////
void Network::OnDelTag(wxCommandEvent &event)
{
  vector<TAG>::iterator iter;
  int answer , i;

  answer=wxMessageBox("Do you really want to delete this tag?", "Confirmation", wxYES_NO);
  if (answer!=wxYES)
    return;
  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);;
  for (iter=tags.begin(), i=0; iter!=tags.end(); iter++, i++)
    if (i==m_selTag)
      {
	tags.erase(iter);
	m_selTag=-1;
	break;
      }
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
  //  Refresh(false);
  ReDrawAll();
}

/////////////////////////////////////////////////////
void Network::OnDelLink(wxCommandEvent &event)
{
  vector<LINK*>::iterator iter;
  vector<LINK*>::iterator iter2;
  int answer, i, j;
  map<int, MODULE>::iterator miter;

  answer=wxMessageBox("Do you really want to delete this link?", "Confirmation", wxYES_NO);
  if (answer!=wxYES)
    return;
  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);;
  for (miter=modules.begin(); miter!=modules.end(); miter++)
    {
      i=miter->first;
      for (iter2=modules[i].links.begin(), j=0; 
	   j<modules[i].links.size(); iter2++, j++)
	if (modules[i].links[j]==links[m_selLink])
	  {
	    modules[i].links.erase(iter2);
	    break;
	  }
    }

  for (iter=links.begin(), i=0; iter!=links.end(); iter++, i++)
    if (i==m_selLink)
      {
	links.erase(iter);
	m_selLink=-1;
	break;
      }
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
  //  Refresh(false);
  ReDrawAll();
}

/////////////////////////////////////////////////////
void Network::OnDelLinkCon(wxCommandEvent &event)
{
  vector<wxPoint>::iterator iter;
  int answer, i;

  answer=wxMessageBox("Do you really want to delete this link connector?", "Confirmation", wxYES_NO);
  if (answer!=wxYES)
    return;

  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);;
  DrawLinkCon(*links[m_selLink], false);

  for (iter=links[m_selLink]->cons.begin(), i=0; iter!=links[m_selLink]->cons.end(); iter++, i++)
    if (i==m_selLinkCon)
      {
	links[m_selLink]->cons.erase(iter);
	links[m_selLink]->poly = CalcLinkPoly((*links[m_selLink]));
	m_selLinkCon=-1;
	break;
      }
  
  DrawLinkCon(*links[m_selLink], true);
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
  ReDrawAll();
}

/////////////////////////////////////////////////////
void Network::OnDelMod(wxCommandEvent &event)
{

  map<int, MODULE>::iterator iter;
  vector<LINK *>::iterator iter2;
  vector<LINK *>::iterator iter3;
  int answer, i, k, l;
  LINK* del_link;

  answer=wxMessageBox("Do you really want to delete this module?", "Confirmation", wxYES_NO);
  if (answer!=wxYES)
    return;

  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);

  //first, delete all the links connects to it
  while (modules[m_selMod].links.size()>0)
    {
      del_link = modules[m_selMod].links[0];
      for (iter=modules.begin(); iter!=modules.end(); iter++)
	{
	  k=iter->first;
	  for (iter2=modules[k].links.begin(), l=0; 
	       l<modules[k].links.size(); iter2++, l++)
	    if (modules[k].links[l]==del_link)
	      {
		modules[k].links.erase(iter2);
		break;
	      }
	}
      for (iter3=links.begin(), k=0; iter3!=links.end(); iter3++, k++)
	if (links[k]==del_link)
	  {
	    links.erase(iter3);
	    del_link = NULL;
	    break;
	  } 
    }

  for (iter=modules.begin(); iter!=modules.end(); iter++)
    if (iter->first==m_selMod)
      {
	delete modules[m_selMod].pl_mod;
	modules[m_selMod].pl_mod=NULL;
	modules.erase(iter);
	m_selLink=-1;
	break;
      }

  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

  ReDrawAll();
}

/////////////////////////////////////
///// Selection Functions ///////////
/////////////////////////////////////

int Network::SelectMod(int x, int y)
{
  wxRect bbox;
  wxPoint pos, temp;
  int i;
  map<int, MODULE>::iterator iter;

  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i=iter->first;
      
      temp.x = x;
      temp.y = y;
      
      if (inside(temp, modules[i].poly))
	{
	  if (m_selMod == i)
	    return i;
	  DrawPorts(modules[i].pl_mod, true);
	  m_selMod = i;
	  return i;
	}
    }

  return -1;
}

/////////////////////////////////////////////////////
void Network::UnSelectMod(wxDC &dc)
{
  DrawPorts(modules[m_selMod].pl_mod, false);
  
  ReDraw(dc);
  m_selMod = -1;
}

///////////////////////////////////////////////////////
int Network::SelectLink(int x, int y)
{
  wxPoint temp;
  int i , j;

  temp.x = x; temp.y = y;
  for (i=0; i<links.size(); i++)
    {
      if (inside(temp, links[i]->poly))
	{
	  DrawLinkCon(*links[i], true);
	  m_selLink = i;
	  return i;
	}
    }
  return -1;
}

//////////////////////////////////////////////////////
void Network::UnSelectLink(wxDC &dc)
{
  DrawLinkCon(*links[m_selLink], false);
  
  ReDraw(dc);
  m_selLink = -1;
  return;
}

//////////////////////////////////////////////////////
int Network::SelectTag(int x, int y)
{
  wxPoint temp;
  int i;
  
  temp.x = x; temp.y = y;
  for (i=0; i<tags.size(); i++)
    {
      if (inside(temp, tags[i].poly))
	{
	  if (m_selTag == i)
	    return i;
	  DrawTagCon(tags[i], true);
	  m_selTag = i;
	  return i;
	}
    }
  return -1;
  
}

/////////////////////////////////////////////////
void Network::UnSelectTag(wxDC &dc)
{
  DrawTagCon(tags[m_selTag], false);
  
  ReDraw(dc);
  m_selTag = -1;
  return;
}

/////////////////////////////////////////////////
/////////////// Misc Functions //////////////////
/////////////////////////////////////////////////

void Network::CleanRect(wxRect box, wxDC &dc)
{
  wxBrush oldbrush = dc.GetBrush();
  wxPen oldpen = dc.GetPen();

  dc.SetBrush(*wxWHITE_BRUSH);
  dc.SetPen(*wxWHITE_PEN);
  
  dc.DrawRectangle(box.x, box.y, box.width, box.height);

  dc.SetBrush(oldbrush);
  dc.SetPen(oldpen);
}

/////////////////////////////////////////////////
wxPoint Network::GetFreePos(wxRect bbox)
{
  int i;
  const int distx=10;
  const int disty=10;
  int limitx = 5;
  int limity = 5;
  int try_x=0;
  int try_y=0;
  wxPoint result(distx,disty);
  wxRect testbox=bbox;

  for (i=0; i<sbboxes.size(); i++)
    {
      testbox.Offset(result.x-testbox.x, result.y-testbox.y);
     
      if (testbox.Intersects(sbboxes[i]))
	{
	  if ((try_y < limity) && (try_y<sbboxes.size()))
	    {
	      result.y=sbboxes[try_y].GetBottom()+disty;
	      try_y++;
	      i=-1;
	      continue;
	    }
	  else if ((try_x < limitx) && (try_x<sbboxes.size()))
	    {
	      result.x=sbboxes[try_x].GetRight()+distx;
	      result.y=disty;
	      try_x++;
	      try_y=0;
	      i=-1;
	      continue;
	    }
	  else
	    {
	      try_y = limity;
	      limity+=5;
	      try_x = 0;
	      limitx+=limitx;
	      result.y=sbboxes[try_y].GetBottom()+disty;
	      try_y++;
	      i=-1;
	      continue;
	    }
	};
    }
 
  return result;

}

///////////////////////////////////////
POLY Network::CalcLinkPoly(LINK l)
{
  wxRect bbox;
  wxPoint pos;
  POLY ports;
  POLY points;
  POLY result;
  int i, num;


  bbox = modules[l.Fr_mod].pl_mod->GetBBox();
  num = modules[l.Fr_mod].pl_mod->GetNumOports();
  ports.resize(num);
  modules[l.Fr_mod].pl_mod->GetOPorts(ports);

  pos.x = bbox.x+ports[l.Fr_port].x;
  pos.y = bbox.y+ports[l.Fr_port].y;

  points.push_back(pos);
  for (i=0; i<l.cons.size(); i++)
    points.push_back(l.cons[i]);

  bbox = modules[l.To_mod].pl_mod->GetBBox();
  
  num = modules[l.To_mod].pl_mod->GetNumIports();
  ports.resize(num);
  modules[l.To_mod].pl_mod->GetIPorts(ports);
  pos.x = bbox.x+ports[l.To_port].x;
  pos.y = bbox.y+ports[l.To_port].y;

  points.push_back(pos);

  for (i=0; i<points.size(); i++)
    result.push_back(wxPoint(points[i].x, points[i].y-3));


  for (i=points.size()-1; i>=0; i--)
    result.push_back(wxPoint(points[i].x, points[i].y+3));
  
  return result;

}

////////////////////////////////////////////////////
POLY Network::CalcTagPoly(TAG t)
{
  wxPoint endpos;
  POLY result;

  endpos.x = t.box.x;
  endpos.y = t.box.y+t.box.height/2;
  
  result.push_back(wxPoint(t.cons[0].x, t.cons[0].y-3));
  result.push_back(wxPoint(t.cons[1].x, t.cons[1].y-3));
  result.push_back(wxPoint(endpos.x, endpos.y-3));
  result.push_back(wxPoint(t.box.x, t.box.y));
  result.push_back(wxPoint(t.box.x+t.box.width, t.box.y));
  result.push_back(wxPoint(t.box.x+t.box.width, t.box.y+t.box.height));
  result.push_back(wxPoint(t.box.x, t.box.y+t.box.height));
  result.push_back(wxPoint(endpos.x, endpos.y+3));
  result.push_back(wxPoint(t.cons[1].x, t.cons[1].y+3));
  result.push_back(wxPoint(t.cons[0].x, t.cons[0].y+3));

  return result;
  
}


////////////////////////////////////////////////////
////////// Move and Drop Functions /////////////////
////////////////////////////////////////////////////

void Network::MoveModule(int x, int y, int mod, wxDC &dc)
{
  REI_Plugin *cur_module;
  wxRect bbox;
  int xunit, yunit;
  int xpos, ypos, oldxpos, oldypos;
  int w, h, ex, ey, sx, sy;
  int i;
  bool scroll = false; 

  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);

  xpos = 1.0 * xpos / m_xUserScale;
  ypos = 1.0 * ypos / m_xUserScale;
  oldxpos = xpos;
  oldypos = ypos;

  sx = 1.0*sx / m_xUserScale;
  sy = 1.0*sy / m_xUserScale;
  w = 1.0*w / m_xUserScale;
  h = 1.0*h / m_xUserScale; 
  
  ex=xpos*xunit+w;
  ey=ypos*yunit+h;

  if (mod < 0) // no module is selected
    return; 
  
  cur_module = modules[mod].pl_mod;
      
  bbox = cur_module->GetBBox(); //Get the Boundoing box of the modul 
  
  if (x>ex-bbox.width)
    xpos+=1.0;//m_xUserScale;
  else  if (x<(ex-w)+relative_pt.x)
    xpos-=1.0;//m_xUserScale;
  
  if (y>ey-bbox.height)
    ypos+=1.0;//m_xUserScale;
  if (y<(ey-h+relative_pt.y))
    ypos-=1.0;//m_xUserScale;
  
  if (x-relative_pt.x+bbox.width > sx)
    {
      nUnitX+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }
  if (y-relative_pt.y+bbox.height > sy)
    {
      nUnitY+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }

  //wipe off the old link connects with this module
  for (i=0; i<modules[mod].links.size(); i++)
    DrawLink(modules[mod].links[i], false);
    
  cur_module->SetPos(wxPoint(x-relative_pt.x, y-relative_pt.y));
  
  if ((bbox.x-3.0/m_xUserScale)>0)
    bbox.x-=3.0/m_xUserScale;
  else
    bbox.x=0;
  
  if ((bbox.y-3.0/m_xUserScale)>0)
    bbox.y-=3.0/m_xUserScale;
  else
    bbox.y=0;
  
  bbox.width+=3.0/m_xUserScale;
  bbox.height+=3.0/m_xUserScale;

  CleanRect(bbox, dc);  
  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = 1.0 * xpos * m_xUserScale;
      ypos = 1.0 * ypos * m_xUserScale;
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
 
}

/////////////////////////////////////////////////////////////
void Network::DropModule(int ix, int iy, int mod, wxDC& dc)
{
  wxRect bbox; //Bounding box  
  int sx, sy;
  double r;
  int vx, vy;
  int x, y;
  int i, num;
  REI_Plugin * cur_module;
  bool scroll = false; 
  POLY tmppoly;	

  //In drag mode
  if (mod < 0) // no module is selected
    return; 
  
  GetVirtualSize(&sx, &sy);
  sx = 1.0*sx / m_xUserScale;
  sy = 1.0*sy / m_xUserScale;
  
  if (ix<relative_pt.x)
    x=relative_pt.x;
  else
    x=ix;
  if (iy<relative_pt.y)
    y=relative_pt.y;
  else
    y=iy;

  cur_module = modules[mod].pl_mod;
      
  bbox = cur_module->GetBBox(); //Get the Boundoing box of the modul 

  //  bbox.x = dc.LogicalToDeviceX(bbox.x);
  //  bbox.y = dc.LogicalToDeviceY(bbox.y);
  bbox.x = 0;
  bbox.y = 0;
  GetViewStart(&vx,&vy);
  //  vx= vx / m_xUserScale;
  //  vy = vy / m_xUserScale;
  
  if (x-relative_pt.x+bbox.width > sx)
    {
      r=(1.0*(x-relative_pt.x+bbox.width) / sx);
      nUnitX=int (r*nUnitX+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vx = nUnitX;
    }
  if (y-relative_pt.y+bbox.height > sy)
    {
      r=(1.0*(y-relative_pt.y+bbox.width) / sy);
      nUnitY=int (r*nUnitY+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vy = nUnitY;
    }
  
  cur_module->SetPos(wxPoint(x-relative_pt.x, y-relative_pt.y));

  num = cur_module->GetNumPoly();
  tmppoly.resize(num);
  cur_module->GetPoly(tmppoly);

  TransPoly(tmppoly, x-relative_pt.x, y-relative_pt.y, modules[mod].poly);

  //Recalce links poly as well
  for (i=0; i<modules[mod].links.size(); i++)
    modules[mod].links[i]->poly = CalcLinkPoly(*modules[mod].links[i]);

  //if ((bbox.x-3)>0)
  //  bbox.x-=3;
  //  else
  //  bbox.x=0;
  
  //if ((bbox.y-3)>0)
  //   bbox.y-=3;
  //else
  //  bbox.y=0;
  
  //  bbox.x =0;
  //  bbox.y =0;
  //  bbox.width=sx;
  //  bbox.height=sy;

  //xpos = 1.0 * xpos * m_xUserScale;
  //ypos = 1.0 * ypos * m_xUserScale;
      
  //  Scroll(vx, vy);  
  //CleanRect(bbox, dc);
  ReDrawAll();

}

/////////////////////////////////////////////////////////////////////////
void Network::TryLink(int x, int y, int mod, int pt, wxDC& dc, bool flag)
{
  int xoff, yoff;
  wxPoint temp;
  POLY ports;
  wxRect bbox;
  static int dest_mod=-1;
  int i, t, num;
  map<int, MODULE>::iterator iter;

  t=-1;

  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i = iter->first;
      temp.x = x;
      temp.y = y;
      if (inside(temp, modules[i].poly) && dest_mod!=mod)
		{
			t = i;
			break;
		}
	}

  iter=modules.find(dest_mod);	
  if (t!=dest_mod && iter!=modules.end())
    DrawPorts(modules[dest_mod].pl_mod, false);
  dest_mod = t;
  
  DrawPorts(modules[mod].pl_mod, false);
  if (flag)
    {
      DrawPorti(modules[mod].pl_mod, pt, flag);
	  num = modules[mod].pl_mod->GetNumIports();
	  ports.resize(num);
      modules[mod].pl_mod->GetIPorts(ports);
      
	  bbox = modules[mod].pl_mod->GetBBox();
      xoff = ports[pt].x+bbox.x;
      yoff = ports[pt].y+bbox.y;
    }
  else
    {
      DrawPorti(modules[mod].pl_mod, pt, flag);
    
	  num = modules[mod].pl_mod->GetNumOports();
	  ports.resize(num);
      modules[mod].pl_mod->GetOPorts(ports);

      bbox = modules[mod].pl_mod->GetBBox();
      xoff = ports[pt].x+bbox.x;
      yoff = ports[pt].y+bbox.y;      
    
    }
      dc.SetPen(*wxWHITE_PEN);
      dc.DrawLine(xoff, yoff, xold, yold);
      ReDraw(dc);
      if (dest_mod >=0)
	DrawPorts(modules[dest_mod].pl_mod, true);
      dc.SetPen(*wxBLACK_PEN);
      dc.DrawLine(xoff, yoff, x, y);
      
      xold = x;
      yold = y;

}

////////////////////////////////////////////////////////////////////////
void Network::DropLink(int x, int y, int mod, int pt, wxDC &dc, bool flag)
{
  //first check if there is an apropriate port on the destination position
  //in the mean time, also find out the wipe off line's start position 
  int xoff, yoff;

  POLY ports;
  wxRect bbox;
  wxPoint temp;
  int dest_mod, dest_port;
  LINK * ln;
  int i, num;
  bool found;
  int index;
  map<int, MODULE>::iterator iter;

  dest_mod = dest_port = -1;

  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i = iter->first;
      temp.x = x;
      temp.y = y;
      if (inside(temp, modules[i].poly))
	{
	  dest_mod = i;
	  break;
	}
    }

  if (dest_mod>=0)
    {
      DrawPorts(modules[dest_mod].pl_mod, false);
      bbox = modules[dest_mod].pl_mod->GetBBox();
  
      temp.x = x - bbox.x;
      temp.y = y - bbox.y;
    }

  if (flag)
    {
      DrawPorts(modules[mod].pl_mod, false); //Wipe off the port rect
      num = modules[mod].pl_mod->GetNumIports();
	  ports.resize(num);
	  modules[mod].pl_mod->GetIPorts(ports);

      bbox = modules[mod].pl_mod->GetBBox();
      xoff = ports[pt].x+bbox.x;
      yoff = ports[pt].y+bbox.y;
      
  if (dest_mod>=0)
	{
      num = modules[dest_mod].pl_mod->GetNumOports();
	  ports.resize(num);
	  modules[dest_mod].pl_mod->GetOPorts(ports);
	  for (i=0; i<ports.size(); i++)
	    if (computenorm(temp, ports[i])<=3)
	      {
		dest_port = i;
		break;
	      }
	}
    }
  else
    {
      DrawPorts(modules[mod].pl_mod, false); //Wipe off the port rect
    
	  num = modules[mod].pl_mod->GetNumOports();
	  ports.resize(num);
      modules[mod].pl_mod->GetOPorts(ports);
      bbox = modules[mod].pl_mod->GetBBox();
      xoff = ports[pt].x+bbox.x;
      yoff = ports[pt].y+bbox.y;     

      // check if the drop point is a out port
      if (dest_mod>=0)
	{
	  num = modules[dest_mod].pl_mod->GetNumIports();	
	  ports.resize(num);
	  modules[dest_mod].pl_mod->GetIPorts(ports);
	  for (i=0; i<ports.size(); i++)
	    if (computenorm(temp, ports[i])<=3)
	      {
		dest_port = i;
		break;
	      }
	}
    }  
  
  //Wipe off the test line
  dc.SetPen(*wxWHITE_PEN);
  dc.DrawLine(xoff, yoff, xold, yold);


  if (dest_mod>=0 && dest_port>=0 && (dest_mod!=mod||dest_port!=pt))
    {
      ln = new LINK;
      if (flag)
	{
	  ln->To_mod = mod;
	  ln->To_port = pt;
	  ln->Fr_mod = dest_mod;
	  ln->Fr_port = dest_port;
	  ln->poly = CalcLinkPoly(*ln);
	}
      else
	{
	  ln->To_mod = dest_mod;
	  ln->To_port = dest_port;
	  ln->Fr_mod = mod;
	  ln->Fr_port = pt;
	  ln->poly = CalcLinkPoly(*ln);
	}
      found = false;
      for (i=0; i<modules[mod].links.size(); i++)
	{
	  if (modules[mod].links[i]->To_mod == ln->To_mod
	      &&modules[mod].links[i]->To_port == ln->To_port
	      &&modules[mod].links[i]->Fr_mod == ln->Fr_mod
	      &&modules[mod].links[i]->Fr_port == ln->Fr_port)
	    {
	      found = true;
	      delete ln;
	    }
	}

      if (!found) // no duplicate links are allowed
	{ 
	  links.push_back(ln);
	  modules[mod].links.push_back(ln); //push_back the index of the link
	  modules[dest_mod].links.push_back(ln);
	}
					    
    }
  
  m_selMod = -1;
  m_selFrPort = -1;
  m_selToPort = -1;
  ReDrawAll();
}

/////////////////////////////////////////////////////////////////////
void Network::MoveLinkCon(int x, int y, int ln, int ln_con, wxDC& dc)
{
  wxPoint cur_con;
  wxRect bbox;
  int xunit, yunit;
  int xpos, ypos, oldxpos, oldypos;
  int w, h, ex, ey, sx, sy;
  bool scroll = false; 

  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);
  
  oldxpos = xpos;
  oldypos = ypos;

  sx = 1.0*sx / m_xUserScale;
  sy = 1.0*sy / m_xUserScale;
  w = 1.0*w / m_xUserScale;
  h = 1.0*h / m_xUserScale; 

  ex=xpos*xunit+w;
  ey=ypos*yunit+h;
      
  if (x>ex)
    xpos+=1;
  if (x<(ex-w))
    xpos-=1;
  
  if (y>ey)
    ypos+=1;
  if (y<(ey-h))
    ypos-=1;
    
  if (x > sx)
    {
      nUnitX+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }
  if (y > sy)
    {
      nUnitY+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }

  //erase the original link;
  DrawLink(links[ln], false);
  DrawLinkCon(*links[ln], false);
  links[ln]->cons[ln_con]=wxPoint(x,y);
 
  if (oldxpos!=xpos || oldypos!=ypos || scroll)
    {
      xpos = 1.0 * xpos * m_xUserScale;
      ypos = 1.0 * ypos * m_xUserScale;
      
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
  DrawLinkCon(*links[ln], true);
  
}

//////////////////////////////////////////////////////////////////////
void Network::DropLinkCon(int x, int y, int ln, int ln_con, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  bool scroll = false; 
  REI_Plugin * cur_module;
  
  GetVirtualSize(&sx, &sy);

  sx = 1.0*sx / m_xUserScale;
  sy = 1.0*sy / m_xUserScale;
  //  w = w / m_xUserScale;
  //h = h / m_xUserScale; 
      
  GetViewStart(&vx,&vy);
  if (x > sx)
    {
      r=(1.0*x/sx);
      nUnitX=int (r*nUnitX+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vx = nUnitX;
    }
  if (y > sy)
    {
      r=(1.0*y/sy);
      nUnitY=int (r*nUnitY+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vy = nUnitY;
    }
  
  links[ln]->cons[ln_con] = wxPoint(x,y);

  links[ln]->poly = CalcLinkPoly(*links[ln]);

  //  Scroll(vx, vy);
  ReDraw(dc);
}

//////////////////////////////////////////////////////////////////
void Network::MoveTagCon(int x, int y, int t, int t_con, wxDC& dc)
{
  wxPoint cur_con;
  wxRect bbox;
  int xunit, yunit;
  int xpos, ypos;
  int oldxpos, oldypos;
  int w, h, ex, ey, sx, sy;
  bool scroll = false; 

  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  oldxpos = xpos;
  oldypos = ypos;
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);
  
  sx = 1.0*sx / m_xUserScale;
  sy = 1.0*sy / m_xUserScale;
  w = 1.0*w / m_xUserScale;
  h = 1.0*h / m_xUserScale; 

  ex=xpos*xunit+w;
  ey=ypos*yunit+h;
      
  if (x>ex)
    xpos+=1;
  if (x<(ex-w))
    xpos-=1;
  
  if (y>ey)
    ypos+=1;
  if (y<(ey-h))
    ypos-=1;
    
  if (x > sx)
    {
      nUnitX+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }
  if (y > sy)
    {
      nUnitY+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }

  //erase the original Tag;
  DrawTag(&tags[t], false);
  DrawTagCon(tags[t], false);
  tags[t].cons[t_con]=wxPoint(x,y);
  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = 1.0 * xpos * m_xUserScale;
      ypos = 1.0 * ypos * m_xUserScale;
      
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
  DrawTagCon(tags[t], true);
  
}

//////////////////////////////////////////////////////////////////
void Network::DropTagCon(int x, int y, int t, int t_con, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  
  REI_Plugin * cur_module;
  bool scroll = false; 

  GetVirtualSize(&sx, &sy);
 
  sx = 1.0*sx / m_xUserScale;
  sy = 1.0*sy / m_xUserScale;
  //  w = w / m_xUserScale;
  //  h = h / m_xUserScale; 
     
  GetViewStart(&vx,&vy);
  if (x > sx)
    {
      r=(1.0*x/sx);
      nUnitX=int (r*nUnitX+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      vx = nUnitX;
      scroll = true;
    }
  if (y > sy)
    {
      r=(1.0*y/sy);
      nUnitY=int (r*nUnitY+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vy = nUnitY;
    }
  
  tags[t].cons[t_con] = wxPoint(x,y);

  tags[t].poly = CalcTagPoly(tags[t]);

  //  Scroll(vx, vy);
  ReDraw(dc);
}

///////////////////////////////////////////////////////
void Network::MoveTag(int x, int y, int t, wxDC &dc)
{
  wxPoint cur_con;
  wxRect bbox;
  int xunit, yunit;
  int xpos, ypos, oldxpos, oldypos;
  int w, h, ex, ey, sx, sy;
  bool scroll=false;
  GetScrollPixelsPerUnit(&xunit, &yunit);
  GetViewStart(&xpos, &ypos);
  GetVirtualSize(&sx, &sy);
  GetClientSize(&w, &h);
  
  oldxpos = xpos;
  oldypos = ypos;

  sx = 1.0*sx / m_xUserScale;
  sy = 1.0*sy / m_xUserScale;
  w = 1.0*w / m_xUserScale;
  h = 1.0*h / m_xUserScale; 

  ex=xpos*xunit+w;
  ey=ypos*yunit+h;

  if (x>ex)
    xpos+=1;
  if (x<(ex-w))
    xpos-=1;
  
  if (y>ey)
    ypos+=1;
  if (y<(ey-h))
    ypos-=1;
    
  if (x > sx)
    {
      nUnitX+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }
  if (y > sy)
    {
      nUnitY+=2;
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
    }

  //erase the original Tag;
  DrawTag(&tags[t],  false);
  DrawTagCon(tags[t], false);

  tags[t].box.x=x-tag_rpt.x;
  tags[t].box.y=y-tag_rpt.y;

  if (oldxpos!=xpos||oldypos!=ypos||scroll)
    {
      xpos = 1.0 * xpos * m_xUserScale;
      ypos = 1.0 * ypos * m_xUserScale;
      
      Scroll(xpos, ypos);
      ReDrawAll();
    }
  else
    ReDraw(dc);
  DrawTagCon(tags[t], true);
  
}

/////////////////////////////////////////////////////
void Network::DropTag(int x, int y, int t, wxDC &dc)
{
  int sx, sy;
  double r;
  int vx, vy;
  bool scroll = false; 
  REI_Plugin * cur_module;
  
  GetVirtualSize(&sx, &sy);

  sx = 1.0*sx / m_xUserScale;
  sy = 1.0*sy / m_xUserScale;
  //  w = w / m_xUserScale;
  //  h = h / m_xUserScale; 
      
  GetViewStart(&vx,&vy);
  if (x > sx)
    {
      r=(1.0*x/sx);
      nUnitX=int (r*nUnitX+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vx = nUnitX;
    }
  if (y > sy)
    {
      r=(1.0*y/sy);
      nUnitY=int (r*nUnitY+1);
      SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );
      scroll = true;
      vy = nUnitY;
    }
  
  tags[t].box.x = x - tag_rpt.x;
  tags[t].box.y = y - tag_rpt.y;

  tags[t].poly = CalcTagPoly(tags[t]);

  //  Scroll(vx, vy);
  ReDraw(dc);
}

//////////////////////////////////////////////////////
///////// Add to Network Funtions ////////////////////
//////////////////////////////////////////////////////

void Network::AddTag(int x, int y, wxString text)
{
  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);;
  TAG t;
  int w, h;
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );

  t.cons[0].x = x; t.cons[0].y = y;
  
  t.cons[1].x = x+60;
  if (y>40)
    t.cons[1].y = y-20;
  else
    t.cons[1].y = y+20;

  dc.GetTextExtent(text, &w, &h);
  
  t.text = text;
  t.box.x=x+80;
  t.box.y=t.cons[1].y - h/2;
  t.box.width = w;
  t.box.height = h;
  t.poly = CalcTagPoly(t);
  tags.push_back(t);
  
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
  ReDraw(dc);
}

//////////////////////////////////////////////////////////////
void Network::AddtoNetwork(REI_Plugin *cur_module, string cls_name)
{
  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);;
  int width, height;
  int x, y;
  MODULE mod;
  POLY tmpPoly;
  wxPoint tmp;
  int i, num;

  wxRect bbox; //Bounding box  

  bbox = cur_module->GetBBox(); //Get the Boundoing box of the modul 

  cur_module->SetPos(GetFreePos(bbox)); //Set the new modules position to be a free space allocated by the network according to its bounding box
  bbox = cur_module->GetBBox();
  mod.pl_mod=cur_module;

  num = cur_module->GetNumPoly();
  tmpPoly.resize(num);
  cur_module->GetPoly(tmpPoly); 

  //for (i=0; i<tmpPoly.size(); i++)
//	  tmp = tmpPoly[i];
  TransPoly(tmpPoly, bbox.x, bbox.y, mod.poly); //Make the network recognize its polygon 
  mod.cls_name = cls_name;

  int id;
  map<int, MODULE>::iterator mit;
  while (1)
    {
      id = wxNewId();
      if (id>9999)
	id=id%9999;
      mit = modules.find(id);
      if (mit==modules.end())
	break;
    };


  modules[id]=mod;

  modules[id].pl_mod->SetID(id);  
  //modules.push_back(mod);
  sbboxes.push_back(bbox);
  //  for (i=0; i<modules.size(); i++)
  
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );
  ReDraw(dc);
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
}

////////////////////////////////////////
/////// Draw Functions /////////////////
////////////////////////////////////////


void Network::ReDrawAll()
{
  wxClientDC dc(this);
  wxRect box;
  PrepareDC(dc);

  //  box.x = box.y=0;
  
  //  box.width = dc.MaxX();
  //  box.height = dc.MaxY();
  //  dc.SetPen(*wxWHITE_PEN);
  //  dc.SetBrush(*wxWHITE_BRUSH);
  //  CleanRect(box, dc); 
  dc.SetUserScale(m_xUserScale, m_yUserScale);
  dc.SetBackground(*wxWHITE_BRUSH);
  dc.Clear();
  dc.SetPen(*wxBLACK_PEN);
  dc.SetBrush(*wxWHITE_BRUSH);
  
  ReDraw(dc);
}

/////////////////////////////////
void Network::ReDraw(wxDC &dc)
{
  int i;

  map<int, MODULE>::iterator iter;
  
  dc.SetPen(*wxBLACK_PEN);
  dc.SetBrush(*wxWHITE_BRUSH);
  dc.SetBackground(*wxWHITE_BRUSH);
  //  dc.Clear();
  //dc.SetBackgroundMode(wxSOLID);

  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i=iter->first;
      modules[i].pl_mod->DrawIcon(&dc);
      modules[i].pl_mod->DrawID(&dc);
    }

  for (i=0; i<links.size(); i++)
    DrawLink(links[i], true);

  for (i=0; i<tags.size(); i++)
    DrawTag(&tags[i], true);
}

///////////////////////////////////////////////////////////////////
void Network::DrawLink(LINK *ln, bool flag)
{ 
  wxRect bbox;
  POLY ports;
  wxPoint * points;
  wxPoint arrow[3];
  int n;
  int i, j, num;
  
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale(m_xUserScale, m_yUserScale);

  wxBrush old_brush = dc.GetBrush();
  wxPen old_pen = dc.GetPen();

  n = ln->cons.size()+2;
  points = new wxPoint[n];

  bbox = modules[ln->To_mod].pl_mod->GetBBox();
  
  num = modules[ln->To_mod].pl_mod->GetNumIports();
  ports.resize(num);
  modules[ln->To_mod].pl_mod->GetIPorts(ports);

  points[0].x = bbox.x+ports[ln->To_port].x;
  points[0].y = bbox.y+ports[ln->To_port].y;

  j=1;
  for (i=ln->cons.size()-1;i>=0; i--, j++)
    points[j]=ln->cons[i];

  bbox = modules[ln->Fr_mod].pl_mod->GetBBox();
  num = modules[ln->Fr_mod].pl_mod->GetNumOports();
  ports.resize(num);
  modules[ln->Fr_mod].pl_mod->GetOPorts(ports);

  points[n-1].x = bbox.x+ports[ln->Fr_port].x;
  points[n-1].y = bbox.y+ports[ln->Fr_port].y;
  
  if (!flag)
    {
      dc.SetPen(*wxWHITE_PEN);
      dc.SetBrush(*wxWHITE_BRUSH);
    }
  else
    {
      dc.SetPen(*wxBLACK_PEN);
      dc.SetBrush(*wxWHITE_BRUSH);
    }
  dc.DrawLines(n, points);

  //Now draw the arrow head
  if (!flag)
    {
      dc.SetPen(*wxWHITE_PEN);
      dc.SetBrush(*wxWHITE_BRUSH);
    }
  else
    {
      dc.SetPen(*wxBLACK_PEN);
      dc.SetBrush(*wxBLACK_BRUSH);
    }
  
  arrow[0]=points[0];
  
  
  double a = atan(3.0/10.0);
  double b = -a;
  double sinb=sin(b); double cosb = cos(b);
  double sina=sin(a); double cosa = cos(a);
  double dist=sqrt(double((points[1].y-points[0].y)*(points[1].y-points[0].y)
		   +(points[1].x-points[0].x)*(points[1].x-points[0].x)));

  arrow[1].x=cosa*12.0/dist*(points[1].x-points[0].x)
    -sina*12.0/dist*(points[1].y-points[0].y)+points[0].x;
  arrow[1].y=sina*12.0/dist*(points[1].x-points[0].x)
    +cosa*12.0/dist*(points[1].y-points[0].y)+points[0].y;

  arrow[2].x=cosb*12.0/dist*(points[1].x-points[0].x)
    -sinb*12.0/dist*(points[1].y-points[0].y)+points[0].x;
  arrow[2].y=sinb*12.0/dist*(points[1].x-points[0].x)
    +cosb*12.0/dist*(points[1].y-points[0].y)+points[0].y;
  
  dc.DrawPolygon(3, arrow);
  dc.SetPen(old_pen);
  dc.SetBrush(old_brush);
  delete [] points; 
}

/////////////////////////////////////////////////////////////
void Network::DrawTag(TAG *t, bool flag)
{
  
  int w, h;
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale(m_xUserScale, m_yUserScale);
  
  wxBrush old_brush = dc.GetBrush();
  wxPen old_pen = dc.GetPen();
  wxPen mypen(old_pen);
  mypen.SetColour("BLUE");
  //  mypen.SetStyle(wxDOT);
  // mypen.SetStyle(wxLONG_DASH);
  wxPoint points[3];

  dc.GetTextExtent(t->text, &w, &h);
  t->box.width = w;
  t->box.height = h;
  t->poly = CalcTagPoly(*t);
  points[0] = t->cons[0];
  points[1] = t->cons[1];
  points[2] = wxPoint(t->box.x, t->box.y+t->box.height/2);
  
  
  if (!flag)
    {
      dc.SetPen(*wxWHITE_PEN);
      dc.SetBrush(*wxWHITE_BRUSH);
    }
  else
    {
      dc.SetPen(mypen);
      dc.SetBrush(*wxWHITE_BRUSH);
    }
 
  fflush(NULL);
  dc.DrawLines(3, points);
  
  
  dc.DrawRectangle(t->box.x-3, t->box.y-3, t->box.width+6, t->box.height+6);


  fflush(NULL);
  if (flag) dc.DrawText(t->text, t->box.x, t->box.y);

  fflush(NULL);
  dc.SetPen(old_pen);
  dc.SetBrush(old_brush);
  
}

/////////////////////////////////////////////////////////////////////
void Network::DrawPorts(REI_Plugin * cur_module, bool flag)
{
  if (!cur_module)
    return;
  POLY ports;
  int i;
  wxRect bbox;
  wxClientDC dc(this);
  wxPoint bport[4];
  wxCoord xoff, yoff;
  int num;
  
  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );

  bport[0]=wxPoint(0,0);
  bport[1]=wxPoint(6,0);
  bport[2]=wxPoint(6,6);
  bport[3]=wxPoint(0,6);

 
  bbox = cur_module->GetBBox();
  wxBrush old_brush=dc.GetBrush();
  wxPen old_pen = dc.GetPen();

  if (flag)
    {
      dc.SetBrush(*wxRED_BRUSH);
      dc.SetPen(*wxBLACK_PEN);
    }
  else
    {
      dc.SetBrush(*wxWHITE_BRUSH);
      dc.SetPen(*wxWHITE_PEN);
    }

  num = cur_module->GetNumIports();
  ports.resize(num);
  cur_module->GetIPorts(ports);
  for (i=0; i<ports.size(); i++)
    { 
      xoff = ports[i].x+bbox.x-3;
      yoff = ports[i].y+bbox.y-3;
      
      dc.DrawPolygon(4, bport, xoff, yoff);      
    }
  if (flag)
    dc.SetBrush(*wxCYAN_BRUSH);
  else
    ; //keep the white brush

  num = cur_module->GetNumOports();
  ports.resize(num);
  cur_module->GetOPorts(ports);

  for (i=0; i<ports.size(); i++)
    { 
      xoff = ports[i].x+bbox.x-3;
      yoff = ports[i].y+bbox.y-3;
      
      dc.DrawPolygon(4, bport, xoff, yoff);      
    }
  
  /* if ((bbox.x-3)>0)
     bbox.x-=3;
     else
     bbox.x=0;
     
     if ((bbox.y-3)>0)
     bbox.y-=3;
     else
     bbox.y=0;
     
     bbox.width+=3;
     bbox.height+=3;
  */
  dc.SetBrush(old_brush);
  dc.SetPen(old_pen);

}

///////////////////////////////////////////////////////////////////////
void Network::DrawPorti(REI_Plugin * cur_module, int index, bool flag)
{
  POLY ports;
  int i, num;

  if (!cur_module)
    return;
  wxClientDC dc(this);
  wxPoint bport[4];
  wxCoord xoff, yoff;
  wxRect bbox;

  PrepareDC(dc);
  dc.SetUserScale( m_xUserScale, m_yUserScale );

  bport[0]=wxPoint(0,0);
  bport[1]=wxPoint(6,0);
  bport[2]=wxPoint(6,6);
  bport[3]=wxPoint(0,6);

 
  bbox = cur_module->GetBBox();
  wxBrush old_brush=dc.GetBrush();

  dc.SetBrush(*wxRED_BRUSH);

  if (flag)
    {
	  num = cur_module->GetNumIports();
      ports.resize(num);
	  cur_module->GetIPorts(ports);
      dc.SetBrush(*wxRED_BRUSH);
    }
  else
    {
	  num = cur_module->GetNumOports();
	  ports.resize(num);
      cur_module->GetOPorts(ports);
      dc.SetBrush(*wxCYAN_BRUSH);
    }

  
  xoff = ports[index].x+bbox.x-3;
  yoff = ports[index].y+bbox.y-3;
      
  dc.DrawPolygon(4, bport, xoff, yoff);      
  
  
  dc.SetBrush(old_brush);
}

/////////////////////////////////////////////////////////
void Network::DrawLinkCon(LINK l, bool flag)
{
  wxPoint bport[4];
  
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale(m_xUserScale, m_yUserScale);

  wxBrush old_brush = dc.GetBrush();
  wxPen old_pen = dc.GetPen();

  wxCoord xoff, yoff;
  POLY linkline, temp;
  wxRect bbox;
  POLY ports;  
  int n, i, j, num;

  bport[0]=wxPoint(0,0);
  bport[1]=wxPoint(6,0);
  bport[2]=wxPoint(6,6);
  bport[3]=wxPoint(0,6);
  
  if (flag)
    {
      dc.SetBrush(*wxGREEN_BRUSH);
      dc.SetPen(*wxBLACK_PEN);
    }
  else
    {
      dc.SetBrush(*wxWHITE_BRUSH);
      dc.SetPen(*wxWHITE_PEN);
    }
  
  if (m_selLink < 0)
    return;

  n = links[m_selLink]->cons.size()+2;
  linkline.resize(n);

  bbox = modules[links[m_selLink]->Fr_mod].pl_mod->GetBBox();
  
  num= modules[links[m_selLink]->Fr_mod].pl_mod->GetNumOports();
  ports.resize(num);
  modules[links[m_selLink]->Fr_mod].pl_mod->GetOPorts(ports);

  linkline[0].x = bbox.x+ports[links[m_selLink]->Fr_port].x;
  linkline[0].y = bbox.y+ports[links[m_selLink]->Fr_port].y;

  for (i=0; i<links[m_selLink]->cons.size(); i++)
    linkline[i+1]=links[m_selLink]->cons[i];

  bbox = modules[links[m_selLink]->To_mod].pl_mod->GetBBox();

  num = modules[links[m_selLink]->To_mod].pl_mod->GetNumIports();
  ports.resize(num);
  modules[links[m_selLink]->To_mod].pl_mod->GetIPorts(ports);
  linkline[n-1].x = bbox.x+ports[links[m_selLink]->To_port].x;
  linkline[n-1].y = bbox.y+ports[links[m_selLink]->To_port].y;

  for (i=0; i<linkline.size(); i++)
    { 
      xoff = linkline[i].x-3;
      yoff = linkline[i].y-3;
      
      dc.DrawPolygon(4, bport, xoff, yoff);      
    }

  dc.SetBrush(old_brush);
  dc.SetPen(old_pen);
}

////////////////////////////////////////////////////////////
void Network::DrawTagCon(TAG t, bool flag)
{
  int i;
  wxPoint bport[4];
  wxClientDC dc(this);
  wxCoord xoff, yoff;

  bport[0]=wxPoint(0,0);
  bport[1]=wxPoint(6,0);
  bport[2]=wxPoint(6,6);
  bport[3]=wxPoint(0,6);
  
  PrepareDC(dc);
  dc.SetUserScale(m_xUserScale, m_yUserScale);

  wxBrush old_brush = dc.GetBrush();
  wxPen old_pen = dc.GetPen();

  if (flag)
    {
      dc.SetBrush(*wxGREEN_BRUSH);
      dc.SetPen(*wxBLACK_PEN);
    }
  else
    {
      dc.SetBrush(*wxWHITE_BRUSH);
      dc.SetPen(*wxWHITE_PEN);
    }
  
  for (i=0; i<2; i++)
    {
      xoff = t.cons[i].x-3;
      yoff = t.cons[i].y-3;
      
      dc.DrawPolygon(4, bport, xoff, yoff);      
    }

  dc.SetBrush(old_brush);
  dc.SetPen(old_pen);
 
}

/////////////////////////////////////////////////////////
////// Math Functions for the points and polygons ///////
/////////////////////////////////////////////////////////

int Network::ccw (wxPoint pt1, wxPoint pt2, wxPoint pt3)
{
  double dx1 = pt2.x - pt1.x;
  double dx2 = pt3.x - pt1.x;
  double dy1 = pt2.y - pt1.y;
  double dy2 = pt3.y - pt1.y;
  
  if(dx1*dy2 > dy1*dx2) return 1;
  if(dx1*dy2 < dy1*dx2) return -1;
  if(dx1*dy2 == dy1*dx2)
    if(dx1*dx2 < 0 || dy1*dy2 < 0) return -1;
    else if(dx1*dx1+dy1*dy1 >= dx2*dx2+dy2*dy2) return 0;
  
  return 1;
}

///////////////////////////////////////////////////////////////
int Network::intersect(POLY l1, POLY l2)
{
  int ccw11 = ccw(l1[0], l1[1], l2[0]);
  int ccw12 = ccw(l1[0], l1[1], l2[1]);
  int ccw21 = ccw(l2[0], l2[1], l1[0]);
  int ccw22 = ccw(l2[0], l2[1], l1[1]);
 
  return(((ccw11*ccw12 < 0) && (ccw21*ccw22 < 0)) ||
	 (ccw11*ccw12*ccw21*ccw22 == 0));
}

//////////////////////////////////////////////////////////////
int Network::inside (wxPoint pt, POLY poly) 
{
  int i, count = 0, j = 0;
  
  POLY lt, lp, lv;
  
  lt.push_back(pt); lt.push_back(pt); lt[1].x = 999999;
  lp.push_back(pt); lp.push_back(pt);
  lv.push_back(pt); lv.push_back(pt);

  wxPoint p(poly.back());
  poly.insert(poly.begin(), 1, p);

  double numsides = poly.size()-1;
  for(i=1; i<=numsides; i++) {
    lp[0] = poly[i];
    lp[1] = poly[i-1];
    if(intersect(lv, lp)) return 1;
    lp[1] = poly[i];
    
    if(!intersect(lp, lt)) {
      lp[1] = poly[j];
      if(intersect(lp, lt)) count++;
      else
        if(i!=j+1 && ((ccw(lt[0], lt[1], poly[j])*(ccw(lt[0], lt[1], poly[i])) < 1)))
	  count++;
      j = i;
    }
  }
  if(j!=numsides && ccw(lt[0], lt[1], poly[j])*ccw(lt[0], lt[1], poly[1]) == 1)
   count--;

  return count & 1;
}

/////////////////////////////////////////////////////////////////
double Network::computenorm (wxPoint pt1, wxPoint pt2)
{
  return sqrt(double((pt1.x - pt2.x)*(pt1.x - pt2.x) + (pt1.y - pt2.y)*(pt1.y - pt2.y)));
}

//////////////////////////////////////////////////////////////////
double Network::nearpnt(wxPoint pt, POLY poly, POLY &Near)
{
  int i, i2;

  double t, d, dist = 99999, numsides = poly.size();

  Near.clear();

  for(i=0; i<numsides; i++) {
    i2 = i+1;
    if(i2 == numsides) i2 = 0;

    wxRealPoint p;
    wxRealPoint v(poly[i2].x-poly[i].x, poly[i2].y-poly[i].y);
    wxRealPoint n(pt.x - poly[i].x, pt.y - poly[i].y);

    t = (n.x*v.x + n.y*v.y) / (v.x*v.x + v.y*v.y);
    if(t <= 0) {
      p.x = poly[i].x;
      p.y = poly[i].y;
    } else if(t >= 1) {
      p.x = poly[i2].x;
      p.y = poly[i2].y;
    } else {
      p.x = poly[i].x+t*v.x;
      p.y = poly[i].y+t*v.y;
    }

    d = computenorm(pt, wxPoint(((int) p.x), ((int) p.y)));
    if(d < dist) {
      Near.clear();
      dist = d;
      Near.push_back(wxPoint(((int) p.x), ((int) p.y)));
    } else if(d==dist)
      Near.push_back(wxPoint(((int) p.x), ((int) p.y)));
  }

  return dist;
}

/////////////////////////////////////////////////////////////
void Network::TransPoly(POLY oldpoly, int x, int y, POLY &newpoly)
{
  int i;
  
  newpoly.clear();  
  for (i=0; i<oldpoly.size(); i++)
    newpoly.push_back(wxPoint(oldpoly[i].x+x, oldpoly[i].y+y));
   
}

//////////////////////////////////////////////
//////// Save and Load Functions /////////////
//////////////////////////////////////////////
void Network::Pack(vector<Interface> & UIs)
{
  // first record the network global variablables
  Interface ntpk; //the network topology and connection pack
  string network_pack;
  char vname[256];
  //module information to be saved
  string modCls;
  
  //link information to be saved
  long lnFrMod, lnToMod, lnFrPort, lnToPort;
  vector<long> lnConX, lnConY;

  //tag information to be saved
  string tagText;
  long tagCon0X, tagCon0Y, tagCon1X, tagCon1Y, tagBoxX, tagBoxY;

  int i,j;
  map<int, MODULE>::iterator iter;

  ntpk._type=0;
  ntpk._category=0;
  ntpk._id=-1;
  ntpk.setVal("m_xUserScale", m_xUserScale);
  ntpk.setVal("m_yUserScale", m_yUserScale);
  ntpk.setVal("nPixX", long(nPixX));
  ntpk.setVal("nPixY", long(nPixY));
  ntpk.setVal("nUnitX", long(nUnitX));
  ntpk.setVal("nUnitY", long(nUnitY));
 
  // second, save the the 3 lists of the modules, links and tags
  ntpk.setVal("Module_size", long(modules.size()));

  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i=iter->first;
      //These are the essential information about a module
      modCls = modules[i].cls_name;
      //poly can be calculated as mod.poly = TransPoly(cur_module->GetPoly(), bbox.x, bbox.y)
      //links vector can be reconstructed from the link's list
      //The order of modules needs to be preserved for the link list and the module UI
      //the UI information of module is packed in different interface packs

      sprintf(vname, "modCls_%.4d", i);
      ntpk.setVal(vname, modCls); // this is string
    }

  ntpk.setVal("Link_size", long(links.size()));
  for (i=0; i<links.size(); i++)
    {
      lnFrMod = links[i]->Fr_mod;
      lnToMod = links[i]->To_mod;
      lnFrPort = links[i]->Fr_port;
      lnToPort = links[i]->To_port;
      sprintf(vname, "ln_FrMod_%.4d", i);
      ntpk.setVal(vname, lnFrMod);
      sprintf(vname, "ln_ToMod_%.4d", i);
      ntpk.setVal(vname, lnToMod);
      sprintf(vname, "ln_FrPort_%.4d", i);
      ntpk.setVal(vname, lnFrPort);
      sprintf(vname, "ln_ToPort_%.4d", i);
      ntpk.setVal(vname, lnToPort);
      
      lnConX.clear();
      lnConY.clear();
      //Try to store link cons,
      //link cons are (x,y) wxpoint
      //here I store x in one vector and y in the other
      for (j=0; j<links[i]->cons.size(); j++)
	{
	  lnConX.push_back(links[i]->cons[j].x);
	  lnConY.push_back(links[i]->cons[j].y);
	}
      sprintf(vname, "ln_ConX_%.4d", i);
      ntpk.setVal(vname, lnConX);
      sprintf(vname, "ln_ConY_%.4d", i);
      ntpk.setVal(vname, lnConY);
    }

  ntpk.setVal("Tag_size", long(tags.size()));
  for (i=0; i<tags.size(); i++)
    {
      tagText = tags[i].text;
      tagCon0X = tags[i].cons[0].x;
      tagCon0Y = tags[i].cons[0].y;
      tagCon1X = tags[i].cons[1].x;
      tagCon1Y = tags[i].cons[1].y;
      tagBoxX = tags[i].box.x;
      tagBoxY = tags[i].box.y;
      sprintf(vname, "tag_Txt_%.4d", i);
      ntpk.setVal(vname, tagText);
      sprintf(vname, "tag_Con0X_%.4d", i);
      ntpk.setVal(vname, tagCon0X);
      sprintf(vname, "tag_Con0Y_%.4d", i);
      ntpk.setVal(vname, tagCon0Y);
      sprintf(vname, "tag_Con1X_%.4d", i);
      ntpk.setVal(vname, tagCon1X);
      sprintf(vname, "tag_Con1Y_%.4d", i);
      ntpk.setVal(vname, tagCon1Y);
      sprintf(vname, "tag_BoxX_%.4d", i);
      ntpk.setVal(vname, tagBoxX);
      sprintf(vname, "tag_BoxY_%.4d", i);
      ntpk.setVal(vname, tagBoxY);
     }

  UIs.clear();
  UIs.push_back(ntpk);

  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i=iter->first;
      modules[i].pl_mod->SetID(i);
      UIs.push_back(*(modules[i].pl_mod->Pack()));
    }

  UIs.push_back(*(globalparam_dlg->Pack()));


}

void Network::UnPack(vector<Interface> & intfs)
{
  int size;
  int _id;
  Interface ntpk;
  vector<string> vars;
  long temp;
  double tempd;
  string temps;
  vector<long> templ1d;
  int pos, i, ii, j, num, polynum;
  wxClassInfo * cls;
  wxRect bbox;
  LINK * ln;
  POLY tmpPoly;
  map<int, MODULE>::iterator iter;
  //Read it from the file
  int modsize;
  MODULE temp_mod;

  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);;

  for (i=0; i< links.size(); i++)
    delete links[i];
  links.clear();

  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i = iter->first;
      delete modules[i].pl_mod;
    }
  modules.clear();

  tags.clear();

  ntpk = intfs[0];

  vars = ntpk.getInts();
  for (i=0; i<vars.size(); i++)
    {
      ntpk.getVal(vars[i], temp);
      if (vars[i]=="nPixX")
	nPixX = temp;
      else if (vars[i]=="nPixY")
	nPixY = temp;
      else if (vars[i]=="nUnitX")
	nUnitX = temp;
      else if (vars[i]=="nUnitY")
	nUnitY = temp;
      else if (vars[i]=="Module_size")
	modsize=temp;
      else if (vars[i]=="Link_size")
	{
	  links.resize(temp); // repopulate the links vector
	  for (j=0; j<temp; j++)
	    {
	      ln = new LINK;
	      links[j]=ln;
	    }
	}
      else if (vars[i]=="Tag_size")
	tags.resize(temp);
      else if ((pos=vars[i].find("ln_FrMod_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+9, 4).c_str());
	  links[num]->Fr_mod=temp;
	}
      else if ((pos=vars[i].find("ln_ToMod_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+9, 4).c_str());
	  links[num]->To_mod=temp;
	}
      else if ((pos=vars[i].find("ln_FrPort_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+10, 4).c_str());
	  links[num]->Fr_port=temp;
	}
      else if ((pos=vars[i].find("ln_ToPort_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+10, 4).c_str());
	  links[num]->To_port=temp;
	}
      else if ((pos=vars[i].find("tag_Con0X_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+10, 4).c_str());
	  tags[num].cons[0].x = temp;
	}
      else if ((pos=vars[i].find("tag_Con0Y_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+10, 4).c_str());
	  tags[num].cons[0].y = temp;
	}
      else if ((pos=vars[i].find("tag_Con1X_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+10, 4).c_str());
	  tags[num].cons[1].x = temp;
	}
      else if ((pos=vars[i].find("tag_Con1Y_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+10, 4).c_str());
	  tags[num].cons[1].y = temp;
	}
      else if ((pos=vars[i].find("tag_BoxX_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+9, 4).c_str());
	  tags[num].box.x = temp;
	}
      else if ((pos=vars[i].find("tag_BoxY_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+9, 4).c_str());
	  tags[num].box.y = temp;
	}
    }

  vars = ntpk.getDoubles();
  for (i=0; i<vars.size(); i++)
    {
      ntpk.getVal(vars[i], tempd);
      if (vars[i]=="m_xUserScale")
	m_xUserScale = tempd;
      else if (vars[i]=="m_yUserScale")
 	m_yUserScale = tempd;
    }

  vars = ntpk.getStrings();
  for (i=0; i<vars.size(); i++)
    {
      ntpk.getVal(vars[i], temps);
      if ((pos=vars[i].find("modCls_"))!=string::npos)
	{
	  num =atoi(vars[i].substr(pos+7, 4).c_str());
	  cls = wxClassInfo::FindClass(temps.c_str());
	  if (cls==NULL)
	    {
	      // wxMessageBox("Load failed : You don't have that class in your Plugin DLL!", temps.c_str());
	      for (ii=0; ii< links.size(); ii++)
		if (links[ii]!=NULL)
		  delete links[ii];
	      links.clear();
	      
	      for (iter=modules.begin(); iter!=modules.end(); iter++)
		{
		  ii = iter->first;
		  if (modules[ii].pl_mod!=NULL)
		    delete modules[ii].pl_mod;
		}
	      modules.clear();
	      
	      tags.clear();
	      while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);
	      return;
	    }

	  modules[num]=temp_mod;
	  modules[num].pl_mod = (REI_Plugin *) cls->CreateObject();
	  modules[num].cls_name = temps;
	}
      if ((pos=vars[i].find("tag_Txt_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+8, 4).c_str());
	  tags[num].text = wxString(temps.c_str());
	}
    }
  vars = ntpk.getInts1D();
  for (i=0; i<vars.size(); i++)
    {
      ntpk.getVal(vars[i],templ1d);
      if ((pos=vars[i].find("ln_ConX_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+8, 4).c_str());

	  if (links[num]->cons.size()==0)
	    links[num]->cons.resize(templ1d.size());

	  for (j=0; j<templ1d.size(); j++)
	    links[num]->cons[j].x = templ1d[j];
	}
      else if ((pos=vars[i].find("ln_ConY_"))!=string::npos)
	{
	  num = atoi(vars[i].substr(pos+8, 4).c_str());
	  for (j=0; j<templ1d.size(); j++)
	    links[num]->cons[j].y = templ1d[j];
	}
    }
  // unpack the modules' UIs


  for (i=0; i<modsize; i++)
    {
      _id = intfs[i+1]._id;
      modules[_id].pl_mod->SetID(_id);
      modules[_id].pl_mod->UnPack(&intfs[i+1]);
    };

    
  //unpack the Global Param Dialog
  //globalparam_dlg->UnPack(&intfs[intfs.size()-1]);

  //Now all the data are read from the file. Let's try to reconstruct the link and the calculate the 
  //first, calculate get the links vector into the modules
  for (i=0; i<links.size(); i++)
    {
      modules[links[i]->To_mod].links.push_back(links[i]);
      modules[links[i]->Fr_mod].links.push_back(links[i]);
    }

  //Second, calculate the polyes
  for (iter=modules.begin(); iter!=modules.end(); iter++)//=0; i<modules.size(); i++)
    {
      i=iter->first;
      bbox = modules[i].pl_mod->GetBBox();
      polynum = modules[i].pl_mod->GetNumPoly();
      tmpPoly.resize(polynum);
      modules[i].pl_mod->GetPoly(tmpPoly);
      TransPoly(tmpPoly, bbox.x, bbox.y, modules[i].poly); //Make the network recognize its polygon 
    }
  
  for (i=0; i<links.size(); i++)
    links[i]->poly = CalcLinkPoly(*(links[i]));

  for (i=0; i<tags.size(); i++)
    tags[i].poly = CalcTagPoly(tags[i]);
  
  m_selMod = -1;
  m_selFrPort = -1; 
  m_selToPort = -1; 
  m_selLink = -1; 
  m_selLinkCon = -1; 
  m_selTag = -1; 
  m_selTagCon = -1; 
  xold = yold =0;
  
  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

  Refresh();
}

void Network::Save(wxString filename)
{
  //Actually write it to file
  vector<Interface> UIs;

  Package p;
  Pack(p.intfs);

  p.SetPackName("Network");
  p.SetSysId(filename.c_str());

  p.Save();

}

////////////////////////////////////////////////////////
void Network::SaveS(std::string &network_pack)
{
  //Actually write it to file
  Package p;
  Pack(p.intfs);

  p.SetPackName("Network");
  p.SetSysId("test.xml");

  bool rv;
  network_pack = p.Save(rv);


}

////////////////////////////////////////////////////////
void Network::New()
{
  int i;
  map<int, MODULE>::iterator iter;

  while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR);;
  
  for (i=0; i< links.size(); i++)
    delete links[i];
  links.clear();
  
  for (iter=modules.begin(); iter!=modules.end(); iter++)
    {
      i = iter->first;
      delete modules[i].pl_mod;
    }
  modules.clear();
  
  tags.clear();

  while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR);

  Refresh();
}

////////////////////////////////////////////////////////
void Network::Load(wxString filename)
{
  Package p;
  p.SetSysId(filename.c_str());
  p.Load();
  UnPack(p.intfs);
 
}

//////////////////////////////////////////////////////
void Network::LoadS(const char* inputs)
{
  Package p;
  p.SetSysId("temp.xml");
  if (string(inputs)!="")
  {
	p.Load(inputs, strlen(inputs));
	UnPack(p.intfs);
  }
}

//////////////////////////////////////////////////////
void  Network::OnShowLinkContent(wxCommandEvent &event)
{
  int mod, port;
  char *linkresult;
  int ipos;
  double temp;
  UIDialog * port_dlg;
  mod = links[m_selLink]->Fr_mod; //The to Mod are actually the from module for the data flow
  port = links[m_selLink]->Fr_port;
  try {
    linkresult = exec->GetExportData(mod, port);
  }
  catch (CORBA::Exception &) {
		
    cerr << "Maybe Engine is down" << endl;
    return;
  }
  
  if (string(linkresult)!=string(""))
    {
      Package p;
      p.SetSysId("linkresult.xml");

      p.Load(linkresult, strlen(linkresult));
      port_dlg = modules[mod].pl_mod->PortData(NULL,  &(p.intfs[0]));
      
      cout<<linkresult<<endl;
      if (port_dlg!=NULL)
	port_dlg->Show();
    }
  
}

//////////////////////////////////////////////////////
void  Network::OnShowResult(wxCommandEvent &event)
{
  char* result;
  
  if (m_selMod<0)
    return;

  if (CORBA::is_nil(exec))
    {
      cerr<<"Not Connected yet!\n";
      return;
    }
  try {
    result = exec->GetModuleResult(m_selMod);
  }
  catch (CORBA::Exception &) {
		
    cerr << "Maybe Computational Engine is down" << endl;
    return;
  }

  cout<<result<<endl;
  if (string(result)!="")
    {
      Package p;
      p.SetSysId("linkresult.xml");
      p.Load(result, strlen(result));

      modules[m_selMod].pl_mod->UnPackResult(&p.intfs[0]);
      UIDialog * hello;
      hello = modules[m_selMod].pl_mod->Result(NULL);
      if (hello!=NULL)
	hello->Show();
    }
}

//////////////////////////////////////////////////////
void Network::OnShowDesc(wxCommandEvent &event)
{
  wxString desc;
  wxString title;
 
  title << wxT("Description");
  
  if (m_selMod<0)
    return;
  
  desc = modules[m_selMod].pl_mod->GetDesc();
  
  wxMessageDialog(this, desc, title).ShowModal();

}

void Network::OnParaView(wxCommandEvent &event)
{
  //wxArrayString output;
  // ::wxExecute("paraview", wxEXEC_ASYNC|wxEXEC_MAKE_GROUP_LEADER);
  //::wxShell("paraview");
  paraThread* para_t=new paraThread(this);
  para_t->Create();
  para_t->Run();
}
