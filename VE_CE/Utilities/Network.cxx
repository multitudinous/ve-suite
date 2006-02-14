/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: Network_Exec.h,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef NETWORK_EXEC_H
#define NETWORK_EXEC_H
Network::Network ()
{
}

Network::~Network ()
{
  clear();
}

void Network::clear ()
{
  unsigned int i;
  for(i=0; i<_module_ptrs.size(); i++) delete _module_ptrs[i];
  for(i=0; i<_connections.size(); i++) delete _connections[i];
  _module_ptrs.clear();
  _connections.clear();
}

int Network::parse( Interface* intf )
{
   int num;
   long int temp;
   unsigned int i, pos;

   std::string temps;
   std::vector<std::string> vars;

   vars = intf->getStrings();
   for (i=0; i<vars.size(); i++) 
   {
      intf->getVal(vars[i], temps);
      if ( vars[i].find("modCls_") != string::npos ) 
      {
         pos=vars[i].find("modCls_");
         num =atoi(vars[i].substr(pos+7, 4).c_str());
         add_module(num, temps);
      }
   }

   std::set<int> links;
   std::map<int, int> FrMod, ToMod, FrPort, ToPort;

   vars = intf->getInts();
   for (i=0; i<vars.size(); i++) 
   {
      intf->getVal(vars[i], temp);

      if ( vars[i].find("ln_FrMod_") != string::npos ) 
      {
         pos=vars[i].find("ln_FrMod_",0,9);
         num = atoi(vars[i].substr(pos+9, 4).c_str());
         FrMod[num] = temp;
         links.insert(num);
      }
      else if ( vars[i].find("ln_ToMod_") != string::npos ) 
      {
         pos=vars[i].find("ln_ToMod_");
         num = atoi(vars[i].substr(pos+9, 4).c_str());
         ToMod[num] = temp;
         links.insert(num);
      }
      else if ( vars[i].find("ln_FrPort_") != string::npos ) 
      {
         pos=vars[i].find("ln_FrPort_");
         num = atoi(vars[i].substr(pos+10, 4).c_str());
         FrPort[num] = temp;; 
         links.insert(num);
      }
      else if ( vars[i].find("ln_ToPort_") != string::npos ) 
      {
         pos=vars[i].find("ln_ToPort_");
         num = atoi(vars[i].substr(pos+10, 4).c_str());
         ToPort[num] = temp;
         links.insert(num);
      }
   }

   if(intf->getInt("Module_size") != (int)_module_ptrs.size()) 
   {
      cerr << "Inconsistent Modules In Network\n";
      return 0;
   }
  
   if(intf->getInt("Link_size")   != (int)links.size()) 
   {
      cerr << "Inconsistent Links In Network\n";
      return 0;
   }

   std::set<int>::iterator iter;
   for(iter=links.begin(); iter!=links.end(); iter++) {

    if(FrMod.find(*iter)==FrMod.end()  || ToMod.find(*iter)==FrMod.end()   ||
       FrPort.find(*iter)==FrMod.end() || ToPort.find(*iter)==FrMod.end()) {
      cerr << "Bad link found\n";
      return  0;
    }

    Connection* cn = new Connection(*iter);
    _connections.push_back(cn);

    if(!addIPort(ToMod[*iter], ToPort[*iter], cn) ||
       !addOPort(FrMod[*iter], FrPort[*iter], cn)) {
      cerr << "Error adding ports\n";
      return 0;
    }
   }

   return 1;
}

int Network::addIPort (int m, int p, Connection* c)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->addIPort(p, c);
  return 1;
}

int Network::addOPort (int m, int p, Connection* c)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->addOPort(p, c);
  return 1;
}

int Network::nmodules ()
{
  return _module_ptrs.size();
}


int Network::module (Module* mod)
{
  int i, fi = -1;

  for(i=0; i<(int)_module_ptrs.size(); i++)
    if(mod->get_id()==_module_ptrs[i]->get_id()) fi = i;

  return fi;
}

Module* Network::module (int idx)
{
  if(idx<0 || idx>=(int)_module_ptrs.size()) return NULL;
  return _module_ptrs[idx];
}

int Network::moduleIdx (int id)
{
  int i, fi = -1;

  for(i=0; i<(int)_module_ptrs.size(); i++)
    if(id==_module_ptrs[i]->get_id()) fi = i;

  return fi;
}

void Network::add_module (int m, std::string name)
{
  int fi = moduleIdx(m);

  if(fi>=0) return;
  
  Module *mod = new Module(m, this);
  mod->_name = name;
  _module_ptrs.push_back(mod);
}  

int Network::getInput (int m, Interface& intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  intf.copy(_module_ptrs[fi]->_inputs);
  return 1;
}

int Network::setInput (int m, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->_inputs.copy(*intf);
  return 1;
}

int Network::getGeomInput(int m, Interface& intf)
{
   int fi = moduleIdx(m);
   if(fi<0) return 0;
   intf.copy(_module_ptrs[fi]->_geominputs);
   return 1;
}

int Network::setGeomInput(int m, Interface* intf)
{
   int fi = moduleIdx(m);
   if(fi<0) return 0;
   _module_ptrs[fi]->_geominputs.copy(*intf);
   return 1;

}

int Network::getOutput (int m, Interface &intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  intf.copy(_module_ptrs[fi]->_outputs);
  return 1;
}

int Network::setOutput (int m, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->_outputs.copy(*intf);
  return 1;
}

int Network::getMessage (int m, Interface &intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  intf.copy(_module_ptrs[fi]->_messages);
  return 1;
}

int Network::setMessage (int m, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->_messages.copy(*intf);
  return 1;
}

int Network::getPortData (int m, int p, Interface& intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->getPortData(p, intf);
}

int Network::setPortData (int m, int p, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->setPortData(p, intf);
}

int Network::getPortProfile (int m, int p, Types::Profile_out& prof)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->getPortProfile(p, prof);
}

int Network::setPortProfile (int m, int p, const Types::Profile* prof)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->setPortProfile(p, prof);
}

////////////////////////////////////////////////////////////////////////////////
