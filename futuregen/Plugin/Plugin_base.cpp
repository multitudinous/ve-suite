
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Plugin_base.h"
#include <iostream>

IMPLEMENT_DYNAMIC_CLASS(REI_Plugin, wxObject)

/////////////////////////////////////////////////////////////////////////////
REI_Plugin::REI_Plugin()
{ 
  dlg=NULL; 
  result_dlg=NULL;
  pos = wxPoint(0,0); //default position
  
  //default shape
  n_pts=5;
  poly = new wxPoint[n_pts];
  
  poly[0]=wxPoint(0,20);
  poly[1]=wxPoint(20,0);
  poly[2]=wxPoint(40,20);
  poly[3]=wxPoint(30, 40);
  poly[4]=wxPoint(10,40);
  
  mod_pack._type = 1 ; //Module
  mod_pack._category = 1; // normal modules
  mod_pack._id = -1;
}

/////////////////////////////////////////////////////////////////////////////
REI_Plugin::~REI_Plugin()
{
  delete [] poly;
  if (dlg!=NULL)
    delete dlg;
  if (result_dlg!=NULL)
    delete dlg;
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetID(int id)
{
  mod_pack._id = id;
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetPos(wxPoint pt)
{
  pos = pt;
}

/////////////////////////////////////////////////////////////////////////////
double REI_Plugin::GetVersion()
{
  return 0.1;
}

/////////////////////////////////////////////////////////////////////////////
wxRect REI_Plugin::GetBBox()
{
  wxRect result;
  int left, right, top, bottom;
  int i;

  //Calculate the Bounding box out the of polygon
  result.SetX(pos.x);
  result.SetY(pos.y);

  if (n_pts==0)
    {
      result.SetWidth(edge_size);
      result.SetHeight(edge_size);
      return result;
    }

  left=poly[0].x;
  right=poly[0].x;
  top=poly[0].y;
  bottom=poly[0].y;
  
  for (i=1; i<n_pts; i++)
    {
      if (left>poly[i].x)
	left=poly[i].x;
      if (right<poly[i].x)
	right=poly[i].x;
      if (top>poly[i].y)
	top=poly[i].y;
      if (bottom<poly[i].y)
	bottom=poly[i].y;
    }
  result.SetWidth(right-left+edge_size);
  result.SetHeight(bottom-top+edge_size);
  return result;	
}

/////////////////////////////////////////////////////////////////////////////
int REI_Plugin::GetNumPoly()
{
	return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::GetPoly(POLY &polygon)
{
  //POLY polygon;
  int i;

  for (i=0; i<n_pts; i++)  
    polygon[i]=poly[i];

  return ;//polygon;
}

/////////////////////////////////////////////////////////////////////////////
int REI_Plugin::GetNumIports()
{
	return 1;
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::GetIPorts(POLY &iports)
{
  
  iports[0]=(poly[0]);
}

/////////////////////////////////////////////////////////////////////////////
int REI_Plugin::GetNumOports()
{
	return 1;
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::GetOPorts(POLY &oports)
{
  int o;

  o = n_pts/2;
  oports[0] = poly[o];
    
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::DrawIcon(wxDC* dc)
{
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::DrawID(wxDC* dc)
{
  //return; // no module id
  int i;
  int x, y;
  int w, h;
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;

  wxString text;

  x=0; y=0;

  for (i=0; i<n_pts; i++)
    {
      x+=poly[i].x;
      y+=poly[i].y;
    }
  x=x/n_pts; y = y/n_pts;

  text<<mod_pack._id;
  dc->GetTextExtent(text, &w, &h);
  dc->DrawText(text, (x-w/2+xoff), (y-h/2+yoff));
  
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* REI_Plugin::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  long new_id;

  new_id = wxNewId();
  //  std::cout<<"New id "<<new_id<<std::endl;
  
  dlg = new UIDialog(parent, new_id, "UIDialog");

  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* REI_Plugin::Result(wxWindow* parent)
{
  std::vector<wxString> titles;
  titles.push_back("Description");
  titles.push_back("Value");

  if (result_dlg!=NULL)
    return result_dlg;
  result_dlg = new TextResultDialog(parent);
  result_dlg->syngas->Clear();
  result_dlg->syngas->AddRow(titles);
  result_dlg->syngas->AddSeperator(' ');
  result_dlg->syngas->AddSeperator('+');
  result_dlg->syngas->AddSeperator(' ');

  return result_dlg;
}

/////////////////////////////////////////////////////////////////////////////
unsigned int REI_Plugin::GetID()
{
  return mod_pack._id;
}

/////////////////////////////////////////////////////////////////////////////
wxString REI_Plugin::GetName()
{
  return _T("Module");
}

/////////////////////////////////////////////////////////////////////////////
wxString REI_Plugin::GetHelp()
{
  return _T("www.reaction-eng.com");  
}

/////////////////////////////////////////////////////////////////////////////
wxString REI_Plugin::GetDesc()
{
  return _T("This is a default module");
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::Lock(bool lock)
{
  if (dlg!=NULL)
    dlg->Lock(lock);
}

////////////////////////////////////////////////////////////////////
Interface* REI_Plugin::Pack()
{
  string result;
  
  map<string, long *>::iterator iteri;
  map<string, double *>::iterator iterd;
  map<string, string *>::iterator iters;
  map<string, vector<long> *>::iterator itervi;
  map<string, vector<double> *>::iterator itervd;
  map<string, vector<string> *>::iterator itervs;


  //printf("mod id : %d\n", mod_pack._id);
  mod_pack.setVal("XPOS",long (pos.x));
  mod_pack.setVal("YPOS",long (pos.y));
  
  for(iteri=_int.begin(); iteri!=_int.end(); iteri++)
    mod_pack.setVal(iteri->first, *(iteri->second));

  for(iterd=_double.begin(); iterd!=_double.end(); iterd++)
    mod_pack.setVal(iterd->first, *(iterd->second));

  for(iters=_string.begin(); iters!=_string.end(); iters++)
    mod_pack.setVal(iters->first, *(iters->second));

  for(itervi=_int1D.begin(); itervi!=_int1D.end(); itervi++)
    mod_pack.setVal(itervi->first, *(itervi->second));

  for(itervd=_double1D.begin(); itervd!=_double1D.end(); itervd++)
    mod_pack.setVal(itervd->first, *(itervd->second));

  for(itervs=_string1D.begin(); itervs!=_string1D.end(); itervs++)
    mod_pack.setVal(itervs->first, *(itervs->second));

  //mod_pack.pack(result);
  
  //wxString wxstr = result.c_str();
  return &mod_pack ;//wxstr;
  
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::UnPack(Interface * intf)
{
  vector<string> vars;
  
  map<string, long *>::iterator iteri;
  map<string, double *>::iterator iterd;
  map<string, string *>::iterator iters;
  map<string, vector<long> *>::iterator itervi;
  map<string, vector<double> *>::iterator itervd;
  map<string, vector<string> *>::iterator itervs;
  
  int i;
  long temp;

  mod_pack = *intf;
  vars = mod_pack.getInts();
  for (i=0; i<vars.size(); i++)
    {
      iteri =_int.find(vars[i]);
      if (iteri!=_int.end())
	mod_pack.getVal(vars[i], *(iteri->second));
      else if (vars[i]=="XPOS")
	{
	  mod_pack.getVal("XPOS", temp);
	  //	  printf("xpos %ld\n", temp);
	  pos.x = temp;
	}
      else if (vars[i]=="YPOS")
	{
	  //	  printf("ypos %ld\n", temp);
	  mod_pack.getVal("YPOS", temp);
	  pos.y = temp;
	}
    }

  vars = mod_pack.getDoubles();
  for (i=0; i<vars.size(); i++)
    {
      iterd =_double.find(vars[i]);
      if (iterd!=_double.end())
	mod_pack.getVal(vars[i], *(iterd->second));
    }  
  
  vars = mod_pack.getStrings();
  for (i=0; i<vars.size(); i++)
    {
      iters =_string.find(vars[i]);
      if (iters!=_string.end())
	mod_pack.getVal(vars[i], *(iters->second));
    }

  vars = mod_pack.getInts1D();
  for (i=0; i<vars.size(); i++)
    {
      itervi =_int1D.find(vars[i]);
      if (itervi!=_int1D.end())
	mod_pack.getVal(vars[i], *(itervi->second));
    }

  vars = mod_pack.getDoubles1D();
  for (i=0; i<vars.size(); i++)
    {
      itervd =_double1D.find(vars[i]);
      if (itervd!=_double1D.end())
	mod_pack.getVal(vars[i], *(itervd->second));
    }

  vars = mod_pack.getStrings1D();
  for (i=0; i<vars.size(); i++)
    {
      itervs =_string1D.find(vars[i]);
      if (itervs!=_string1D.end())
	mod_pack.getVal(vars[i], *(itervs->second));
    }


}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::UnPackResult(Interface* intf)
{
  //This will be module dependent. here is the Default implementation when using the summary table to pack things up in the module end
  int i;
  std::vector<string> descs;
  descs = mod_pack.getStrings();
  v_desc.clear();
  v_value.clear();
  for (i=0; i<descs.size(); i++)
    {
      v_desc.push_back(descs[i].c_str());
      v_value.push_back((intf->getString(descs[i])).c_str());
    }
  
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::RegistVar(string vname, long *var)
{
  _int[vname]=var;
}

void REI_Plugin::RegistVar(string vname, double *var)
{
  _double[vname]=var;
}

void REI_Plugin::RegistVar(string vname, string *var)
{
  _string[vname]=var;
}

void REI_Plugin::RegistVar(string vname, vector<long> *var)
{
  _int1D[vname]=var;
}

void REI_Plugin::RegistVar(string vname, vector<double> *var)
{
  _double1D[vname]=var;
}

void REI_Plugin::RegistVar(string vname, vector<string> *var)
{
  _string1D[vname]=var;
}
