#include "VE_Conductor/Framework/Plugin_base.h"
#include <iostream>
#include "VE_Conductor/Framework/string_ops.h"
#include "VE_Conductor/Framework/Geometry.h"
#include "VE_Conductor/Framework/UIDialog.h"
#include "VE_Conductor/Framework/TextResultDialog.h"
// EPRI TAG
#include "VE_Conductor/Framework/FinancialDialog.h"
#include "VE_Conductor/Framework/GeometryDialog.h"
#include "VE_Conductor/Framework/TexTable.h"
#include "VE_Conductor/Framework/GeometryDataBuffer.h"

IMPLEMENT_DYNAMIC_CLASS( REI_Plugin, wxObject )

/////////////////////////////////////////////////////////////////////////////
REI_Plugin::REI_Plugin()
{ 
   dlg         = 0; 
   result_dlg  = 0;
   port_dlg    = 0;
   geom_dlg     = 0;
   geometryDataBuffer = 0;
   // EPRI TAG
   financial_dlg = 0;

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
   poly = 0;

   if (dlg!=NULL)
   {       
      delete dlg;
      dlg = 0;
   }
   
   if (result_dlg!=NULL)
   {
      delete result_dlg;
      result_dlg = 0;
   }
   
   if (port_dlg!=NULL)   
   {
      delete port_dlg;
      port_dlg = 0;
   }

   if ( geom_dlg!=NULL )
   {
      delete geom_dlg;
      geom_dlg = 0;
   }

   if ( geometryDataBuffer!=NULL )
   {
      delete geometryDataBuffer;
      geometryDataBuffer = 0;
   }

   // EPRI TAG
   if (financial_dlg!=NULL) 
   {
      delete financial_dlg;
      financial_dlg = 0;
   }
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::SetID(int id)
{
  mod_pack._id = id;

  if ( geometryDataBuffer == 0 )
      geometryDataBuffer = new GeometryDataBuffer();
  geometryDataBuffer->SetCurrentModuleID(id);
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

  if (result_dlg==NULL)
    result_dlg = new TextResultDialog(parent);
  result_dlg->syngas->Clear();
  result_dlg->syngas->AddRow(titles);
  result_dlg->syngas->AddSeperator(' ');
  result_dlg->syngas->AddSeperator('+');
  result_dlg->syngas->AddSeperator(' ');
  result_dlg->Set2Cols(v_desc, v_value);
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

/////////////////////////////////////////////////////////////////////////////
bool REI_Plugin::Has3Ddata()
{
  return false;
}

////////////////////////////////////////////////////////////////////
Interface* REI_Plugin::Pack()
{
  //string result;
  
  std::map<std::string, long *>::iterator iteri;
  std::map<std::string, double *>::iterator iterd;
  std::map<std::string, std::string *>::iterator iters;
  std::map<std::string, std::vector<long> *>::iterator itervi;
  std::map<std::string, std::vector<double> *>::iterator itervd;
  std::map<std::string, std::vector<std::string> *>::iterator itervs;


  //std::cout << "mod id : " << mod_pack._id << std::endl;
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
  {
	std::vector<std::string> * y;
	std::string x;
	x=itervs->first;
	y=itervs->second;
    mod_pack.setVal(itervs->first, *(itervs->second));
  }

  // EPRI TAG
  if(financial_dlg != NULL) {
    mod_pack.setVal("USE_FINANCIAL", (long)financial_dlg->_use_data);

    mod_pack.setVal("CC00", financial_dlg->_cc00_d);
    mod_pack.setVal("CC01", financial_dlg->_cc01_d);
    mod_pack.setVal("CC02", financial_dlg->_cc02_d);
    mod_pack.setVal("CC03", financial_dlg->_cc03_d);
    mod_pack.setVal("CC04", financial_dlg->_cc04_d);
    mod_pack.setVal("CC05", financial_dlg->_cc05_d);
    mod_pack.setVal("CC06", financial_dlg->_cc06_d);
    mod_pack.setVal("CC07", financial_dlg->_cc07_d);
    mod_pack.setVal("CC08", financial_dlg->_cc08_d);

    mod_pack.setVal("OM00", financial_dlg->_om00_d);
    mod_pack.setVal("OM01", financial_dlg->_om01_d);
    mod_pack.setVal("OM02", financial_dlg->_om02_d);
    mod_pack.setVal("OM03", financial_dlg->_om03_d);
  }

  //mod_pack.pack(result);
  
  //wxString wxstr = result.c_str();
 
  return &mod_pack ;//wxstr;
  
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::UnPack(Interface * intf)
{
  std::vector<std::string> vars;
  
  std::map<std::string, long *>::iterator iteri;
  std::map<std::string, double *>::iterator iterd;
  std::map<std::string, std::string *>::iterator iters;
  std::map<std::string, std::vector<long> *>::iterator itervi;
  std::map<std::string, std::vector<double> *>::iterator itervd;
  std::map<std::string, std::vector<std::string> *>::iterator itervs;
  
  unsigned int i;
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
	  //	  std::cout << "xpos " << temp << std::endl;
	  pos.x = temp;
	}
      else if (vars[i]=="YPOS")
	{
	  //	  std::cout << "ypos " << temp << std::endl;
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
		if ( i == 2 )
			std::cout << " " << std::endl;
      itervs =_string1D.find(vars[i]);
      if (itervs!=_string1D.end())
	  {
		  std::cout<< (*itervs->second).size()<< std::endl;
		  unsigned int testInt = (*itervs->second).size();
		  unsigned int idint = this->GetID();
		  std::string tempstring = vars[i];
		  std::vector<std::string> tempvector = (*itervs->second);
		  //(*itervs->second).clear();
	mod_pack.getVal(vars[i], tempvector );
    unsigned int tempInt = tempvector.size();
	(*itervs->second) = tempvector;
	  }
    }

  // EPRI TAG
  long uf = 0;
  if(mod_pack.getVal("USE_FINANCIAL", uf)) {
    
    if(financial_dlg == NULL)
      financial_dlg = new FinancialDialog (NULL, (wxWindowID)-1);
    
    financial_dlg->_use_data = uf;
    
    financial_dlg->_cc00_d = mod_pack.getDouble("CC00");
    financial_dlg->_cc01_d = mod_pack.getDouble("CC01");
    financial_dlg->_cc02_d = mod_pack.getDouble("CC02");
    financial_dlg->_cc03_d = mod_pack.getDouble("CC03");
    financial_dlg->_cc04_d = mod_pack.getDouble("CC04");
    financial_dlg->_cc05_d = mod_pack.getDouble("CC05");
    financial_dlg->_cc06_d = mod_pack.getDouble("CC06");
    financial_dlg->_cc07_d = mod_pack.getDouble("CC07");
    financial_dlg->_cc08_d = mod_pack.getDouble("CC08");

    financial_dlg->_om00_d = mod_pack.getDouble("OM00");
    financial_dlg->_om01_d = mod_pack.getDouble("OM01");
    financial_dlg->_om02_d = mod_pack.getDouble("OM02");
    financial_dlg->_om03_d = mod_pack.getDouble("OM03");
  }
  
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::UnPackResult(Interface* intf)
{
   //This will be module dependent. 
   //here is the Default implementation when using the 
   //summary table to pack things up in the module end
  
   std::vector<std::string> descs;
   descs = intf->getStrings();
   v_desc.clear();
   v_value.clear();
   for ( unsigned int i=0; i<descs.size(); ++i )
   {
      v_desc.push_back( descs[i].c_str() );
      v_value.push_back( (intf->getString(descs[i])).c_str() );
   }
}

/////////////////////////////////////////////////////////////////////////////
void REI_Plugin::RegistVar(std::string vname, long *var)
{
  _int[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, double *var)
{
  _double[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, std::string *var)
{
  _string[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, std::vector<long> *var)
{
  _int1D[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, std::vector<double> *var)
{
  _double1D[vname]=var;
}

void REI_Plugin::RegistVar(std::string vname, std::vector<std::string> *var)
{
  _string1D[vname]=var;
}

///////////////////////////////////////////////
UIDialog* REI_Plugin::PortData(wxWindow* parent,  Interface *it)
{
  //This default implementation is for the Gas and water Interface's Data package
  //New modules can override the function to implement its own port dialog
  std::vector<wxString> titles;
  titles.push_back("Description");
  titles.push_back("Value");

  std::vector<wxString> gas_desc;
  std::vector<wxString> gas_value;
  unsigned int i;
  //first, to decide if it is water or gas

   bool ok = true;
   it->getDouble("ENTHALPY",    &ok);
   if (!ok)
   { //gas
      gas_desc.push_back("COALCAL");
      gas_value.push_back(to_string(it->getDouble("COALCAL")).c_str());
      gas_desc.push_back("ASHCAL");
      gas_value.push_back(to_string(it->getDouble("ASHCAL")).c_str());
      gas_desc.push_back("ASHPH");
      gas_value.push_back(to_string(it->getDouble("ASHPH")).c_str());
      gas_desc.push_back("PRESSURE_DROP");
      gas_value.push_back(to_string(it->getDouble("PRESSURE_DROP")).c_str());
      gas_desc.push_back("TEMPERATURE");
      gas_value.push_back(to_string(it->getDouble("TEMPERATURE")).c_str());
	   gas_desc.push_back("MW");
      gas_value.push_back(to_string(it->getDouble("MW")).c_str());
      gas_desc.push_back("PRESSURE");
      gas_value.push_back(to_string(it->getDouble("PRESSURE")).c_str());
      gas_desc.push_back("FLOWRATE");
      gas_value.push_back(to_string(it->getDouble("FLOWRATE")).c_str());
      gas_desc.push_back("TAR");
      gas_value.push_back(to_string(it->getDouble("TAR")).c_str());
      gas_desc.push_back("SOOT");
      gas_value.push_back(to_string(it->getDouble("SOOT")).c_str());
      gas_desc.push_back("PT MEAN_SIZE");
      gas_value.push_back(to_string(it->getDouble("MEAN_SIZE")).c_str());
      gas_desc.push_back("PT SIZE_VARIANCE");
      gas_value.push_back(to_string(it->getDouble("SIZE_VARIANCE")).c_str());
      gas_desc.push_back("PARTICLE T");
      gas_value.push_back(to_string(it->getDouble("T_PARTICLE")).c_str());
      gas_desc.push_back("PARITCLE M");
      gas_value.push_back(to_string(it->getDouble("M_PARTICLE")).c_str());
      gas_desc.push_back("COMP NAME");
      gas_value.push_back("COMP CONC");
      
      std::vector<std::string> comp_name = it->getString1D("COMP_NAME");
      std::vector<double>      comp_conc = it->getDouble1D("COMP_CONC");
      for (i=0; i<comp_name.size(); i++)
	   {
	      gas_desc.push_back(comp_name[i].c_str());
	      gas_value.push_back(to_string(comp_conc[i]).c_str());
	   }
      
      gas_desc.push_back("PART NAME");
      gas_value.push_back("PART CONC");

      std::vector<std::string> part_name = it->getString1D("PART_NAME");
      std::vector<double>      part_conc = it->getDouble1D("PART_CONC");
      for (i=0; i<part_name.size(); i++)
	   {
	      gas_desc.push_back(part_name[i].c_str());
	      gas_value.push_back(to_string(part_conc[i]).c_str());
	   }
      
      gas_desc.push_back("WIC NAME");
      gas_value.push_back("WIC CONC");
      std::vector<std::string> wic_name = it->getString1D("WIC_NAME", &ok);
      std::vector<double>      wic_conc = it->getDouble1D("WIC_CONC", &ok);
      for (i=0; i<wic_name.size(); i++)
	   {
	      gas_desc.push_back(wic_name[i].c_str());
	      gas_value.push_back(to_string(wic_conc[i]).c_str());
	   }
   }
   else
   { //water
      gas_desc.push_back("TEMPERATURE");
      gas_value.push_back(to_string(it->getDouble("TEMPERATURE")).c_str());
      gas_desc.push_back("PRESSURE");
      gas_value.push_back(to_string(it->getDouble("PRESSURE")).c_str());
      gas_desc.push_back("ENTHALPY");
      gas_value.push_back(to_string(it->getDouble("ENTHALPY")).c_str());
      gas_desc.push_back("QUALITY");
      gas_value.push_back(to_string(it->getDouble("QUALITY")).c_str());
      gas_desc.push_back("FLOWRATE");
      gas_value.push_back(to_string(it->getDouble("FLOWRATE")).c_str());
    
   }

   if (port_dlg==NULL)
      port_dlg = new TextResultDialog(parent);
  
   port_dlg->syngas->Clear();
   port_dlg->syngas->AddRow(titles);
   port_dlg->syngas->AddSeperator(' ');
   port_dlg->syngas->AddSeperator('+');
   port_dlg->syngas->AddSeperator(' ');
   port_dlg->Set2Cols(gas_desc, gas_value);

   return port_dlg;
}

// EPRI TAG
///////////////////////////////////////////////
void REI_Plugin::FinancialData ()
{
  if(financial_dlg == NULL)
    financial_dlg = new FinancialDialog (NULL, (wxWindowID)-1);
  
  financial_dlg->Show();
}

// Gui to input geometry data

void REI_Plugin::SetIDtoGeometryDataBuffer()
{
  if ( geometryDataBuffer == 0 )
      geometryDataBuffer = new GeometryDataBuffer();

   geometryDataBuffer->SetCurrentModuleID(this->GetID());
}

void REI_Plugin::GeometryData()
{
   geom_dlg = new GeometryDialog(NULL);

   geom_dlg->Show();
}
/*
// return pointer to geometry dialog
GeometryDialog* REI_Plugin::GetGeometryDialog( void )
{
   return geom_dlg;
}
*/
GeometryDataBuffer* REI_Plugin::GetGeometryDataBuffer( void )
{
   if ( geometryDataBuffer == 0 )
      geometryDataBuffer = new GeometryDataBuffer();

   return geometryDataBuffer;
}

bool REI_Plugin::HasGeomInfoPackage()
{
   int local_id = this->GetID();
   std::map<int, std::vector <GeometryInfoPackage> > localmap;

   if ( geometryDataBuffer == 0 )
      geometryDataBuffer = new GeometryDataBuffer();

   localmap = geometryDataBuffer->GetWholeGeomInfoMap();

   std::map<int, std::vector <GeometryInfoPackage> >::iterator itr;
   itr =localmap.find(local_id);
   if(itr!=localmap.end())
   {
      return true;
   }
   else
   {
      return false;
   }
   
}


