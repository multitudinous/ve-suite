//////////////////////////////////////////////
//////// Save and Load Functions /////////////
//////////////////////////////////////////////
void Network::Save( DOMDocument* doc )
{
   // Here we wshould loop over all of the following
   //  Newtork
   doc->getDocumentElement()->appendChild
         (
            VE_Model::Network->GetXMLData( "vecommand" );
         );

   //  Models
   std::map<int, MODULE>::iterator iter;
   for ( iter=modules.begin(); iter!=modules.end(); ++iter )
   {
      modules[ iter->first ].pl_mod->SetID(i);
      doc->getDocumentElement()->appendChild
         ( 
            modules[ iter->first ].pl_mod->GetVEModel()->GetXMLData( "vecommand" )
         );
   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      UIs.push_back(*(modules[i].pl_mod->Pack()));
   }
   }
   //  Canvas info
   doc->getDocumentElement()->appendChild
         (
            canvas->classinfo->GetXMLData( "vecommand" );
         );
  ntpk.setVal("m_xUserScale", m_xUserScale);
  ntpk.setVal("m_yUserScale", m_yUserScale);
  ntpk.setVal("nPixX", long(nPixX));
  ntpk.setVal("nPixY", long(nPixY));
  ntpk.setVal("nUnitX", long(nUnitX));
  ntpk.setVal("nUnitY", long(nUnitY));

   //  tags
   for ( size_t i = 0; i < tags.size(); ++i )
   {
      std::pair< unsigned int, unsigned int > pointCoords;

      veTagVector.push_back( new VE_Model::Tag( doc ) );

      veTagVector.back()->SetTagText( tags.back().text.c_str() );

      pointCoords.first = tags.back().cons[0].x;
      pointCoords.second = tags.back().cons[0].y;
      veTagVector.back()->GetTagPoint( 0 )->SetPoint( pointCoords );

      pointCoords.first = tags.back().cons[1].x;
      pointCoords.second = tags.back().cons[1].y;
      veTagVector.back()->GetTagPoint( 1 )->SetPoint( pointCoords );

      pointCoords.first = tags.back().box.x;
      pointCoords.second = tags.back().box.y;
      veTagVector.back()->GetTagPoint( 2 )->SetPoint( pointCoords );
   }

   for ( size_t i = 0; i < tags.size(); ++i )
   {
      doc->getDocumentElement()->appendChild
         ( 
            veTagVector.at( i )->GetXMLData( "veTag" )
         );
   }
}

////////////////////////////////////////////////////////
void Network::Load( DOMDocument* doc )
{
   // Load from the nt file loaded through wx
   // Get a list of all the command elements

   // do this for network
   VE_Model::Network* veNetwork = new VE_Model::Netowrk( doc );
   veNetwork->SetObjectFromXMLData
               ( 
                  dynamic_cast< DOMElement* >( doc->getDocumentElement()
                                                   ->getElementsByTagName( xercesString("veNetwork") )
                                                   ->item( 0 ) ) 
               );
   unsigned int numCommands = subElements->getLength();
   // now lets create a list of them
   for ( unsigned int i = 0; i < numCommands; ++i )
   {
      Command* temp = new Command( doc );
      temp->SetObjectFromXMLData( dynamic_cast< DOMElement* >( subElements->item(i) ) );
      commandVectorQueue.push_back( temp );
  ntpk.setVal("m_xUserScale", m_xUserScale);
  ntpk.setVal("m_yUserScale", m_yUserScale);
  ntpk.setVal("nPixX", long(nPixX));
  ntpk.setVal("nPixY", long(nPixY));
  ntpk.setVal("nUnitX", long(nUnitX));
  ntpk.setVal("nUnitY", long(nUnitY));
   }

   // do this for models
   DOMNodeList* subElements = doc->getDocumentElement()->getElementsByTagName( xercesString("vecommand") );
   unsigned int numCommands = subElements->getLength();
   // now lets create a list of them
   for ( unsigned int i = 0; i < numCommands; ++i )
   {
      Command* temp = new Command( doc );
      temp->SetObjectFromXMLData( dynamic_cast< DOMElement* >( subElements->item(i) ) );
      commandVectorQueue.push_back( temp );
  std::map<int, MODULE> modules; //The list of modules;
   }

   // do this for tags
   DOMNodeList* subElements = doc->getDocumentElement()->getElementsByTagName( xercesString("veTag") );
   unsigned int numTags = subElements->getLength();
   // now lets create a list of them
   for ( unsigned int i = 0; i < numCommands; ++i )
   {
      VE_Model::Tag* temp = new VE_Model::Tag( doc );
      temp->SetObjectFromXMLData( dynamic_cast< DOMElement* >( subElements->item(i) ) );
      veTagVector.push_back( temp );
      tags.push_back( TAG );
      tags.back().text = wxString( veTagVector.back()->GetTagText().c_str() );
      tags.back().cons[0].x = veTagVector.back()->GetTagPoint( 0 )->GetPoint().first;
      tags.back().cons[0].y = veTagVector.back()->GetTagPoint( 0 )->GetPoint().second;
      tags.back().cons[1].x = veTagVector.back()->GetTagPoint( 1 )->GetPoint().first;
      tags.back().cons[1].y = veTagVector.back()->GetTagPoint( 1 )->GetPoint().second;
      tags.back().box.x = veTagVector.back()->GetTagPoint( 2 )->GetPoint().first;
      tags.back().box.x = veTagVector.back()->GetTagPoint( 2 )->GetPoint().second;
   }

//////////////////////////////////////////
//Now do initialization stuff
   //Now all the data are read from the file. 
   //Let's try to reconstruct the link and the calculate the 
   //first, calculate get the links vector into the modules
   for ( size_t i=0; i<links.size(); i++)
   {
      modules[ links[i]->To_mod ].links.push_back( links[i] );
      modules[ links[i]->Fr_mod ].links.push_back( links[i] );
   }

   //Second, calculate the polyes
   std::map<int, MODULE>::iterator iter;
   for (iter=modules.begin(); iter!=modules.end(); iter++)//=0; i<modules.size(); i++)
   {
      bbox = modules[ iter->first ].pl_mod->GetBBox();
      polynum = modules[ iter->first ].pl_mod->GetNumPoly();
      tmpPoly.resize(polynum);
      modules[ iter->first ].pl_mod->GetPoly(tmpPoly);
      TransPoly(tmpPoly, bbox.x, bbox.y, modules[ iter->first ].poly); //Make the network recognize its polygon 
   }
  
   // Create the polygon for links
   for ( size_t i=0; i < links.size(); ++i )
      links[ i ]->poly = CalcLinkPoly( *(links[i]) );

   // Create the polygon for tags
   for ( size_t i=0; i < tags.size(); ++i )
      tags[i].poly = CalcTagPoly(tags[i]);

   m_selMod = -1;
   m_selFrPort = -1; 
   m_selToPort = -1; 
   m_selLink = -1; 
   m_selLinkCon = -1; 
   m_selTag = -1; 
   m_selTagCon = -1; 
   xold = yold =0;

   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR){ ; }

   Refresh();
}

//////////////////////////////////////////////////////
void Network::Pack(std::vector<Interface> & UIs)
{
  // first record the network global variablables
  Interface ntpk; //the network topology and connection pack
  std::string network_pack;
  //char* vname;
  //module information to be saved
  std::string modCls;
  
  //link information to be saved
  long lnFrMod, lnToMod, lnFrPort, lnToPort;
  std::vector<long> lnConX, lnConY;

  //tag information to be saved
  std::string tagText;
  long tagCon0X, tagCon0Y, tagCon1X, tagCon1Y, tagBoxX, tagBoxY;

  int i,j;
  std::map<int, MODULE>::iterator iter;

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

	   std::ostringstream dirStringStream;
	   dirStringStream << "modCls_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), modCls); // this is string
   }

   ntpk.setVal("Link_size", long(links.size()));
   for (i=0; i<(int)links.size(); i++)
   {
      lnFrMod = links[i]->Fr_mod;
      lnToMod = links[i]->To_mod;
      lnFrPort = links[i]->Fr_port;
      lnToPort = links[i]->To_port;

      std::ostringstream dirStringStream;
      dirStringStream << "ln_FrMod_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnFrMod);
      dirStringStream.str("");
      dirStringStream.clear();

	   dirStringStream << "ln_ToMod_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnToMod);
      dirStringStream.str("");
      dirStringStream.clear();

	   dirStringStream << "ln_FrPort_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnFrPort);
      dirStringStream.str("");
      dirStringStream.clear();

	   dirStringStream << "ln_ToPort_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnToPort);
      dirStringStream.str("");
      dirStringStream.clear();
      
      lnConX.clear();
      lnConY.clear();
      //Try to store link cons,
      //link cons are (x,y) wxpoint
      //here I store x in one vector and y in the other
      for (j=0; j<(int)(links[i]->cons.size()); j++)
	   {
	      lnConX.push_back(links[i]->cons[j].x);
	      lnConY.push_back(links[i]->cons[j].y);
	   }

	   dirStringStream << "ln_ConX_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnConX);
      dirStringStream.str("");
      dirStringStream.clear();

	   dirStringStream << "ln_ConY_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), lnConY);
      dirStringStream.str("");
      dirStringStream.clear();
   }

   ntpk.setVal("Tag_size", long(tags.size()));
   for (i=0; i<(int)tags.size(); i++)
   {
      tagText = tags[i].text;
      tagCon0X = tags[i].cons[0].x;
      tagCon0Y = tags[i].cons[0].y;
      tagCon1X = tags[i].cons[1].x;
      tagCon1Y = tags[i].cons[1].y;
      tagBoxX = tags[i].box.x;
      tagBoxY = tags[i].box.y;
	  
      std::ostringstream dirStringStream;
      dirStringStream << "tag_Txt_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagText);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_Con0X_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagCon0X);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_Con0Y_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagCon0Y);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_Con1X_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagCon1X);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_Con1Y_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagCon1Y);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_BoxX_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagBoxX);
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << "tag_BoxY_" << std::setprecision(4) << std::setfill( '0' ) << i;
      ntpk.setVal(dirStringStream.str(), tagBoxY);
      dirStringStream.str("");
      dirStringStream.clear();
   }

   UIs.clear();
   UIs.push_back(ntpk);

   for (iter=modules.begin(); iter!=modules.end(); iter++)
   {
      i=iter->first;
      modules[i].pl_mod->SetID(i);
      UIs.push_back(*(modules[i].pl_mod->Pack()));
      //if module has geometry data
      // then grab geometry interface
      // call spcific modules geom pack
      if ( modules[i].pl_mod->HasGeomInfoPackage() )
      {
         //modules[i].pl_mod->GetGeometryInfoPackage()->SetID(i);
         //UIs.push_back( *(modules[i].pl_mod->GetGeometryInfoPackage()->Pack()) );
         Geometry* geometry = new Geometry( modules[ i ].pl_mod->GetID() );
         geometry->SetGeometryDataBuffer( modules[ i ].pl_mod->GetGeometryDataBuffer() );

         UIs.push_back( *(geometry->Pack()));

         delete geometry;
      }
   }

   // Pack up global data
   // This is commented out because the computational
   // engine does not have the capability to handle
   // global data yet. This functionality should
   // be addressed shortly to handle this type of data
   // in the framework.
   //UIs.push_back( *(globalparam_dlg->Pack()) );
}

void Network::UnPack(std::vector<Interface> & intfs)
{
   int _id = 0;
   Interface ntpk;
   std::vector<std::string> vars;
   long temp = 0;
   double tempd = 0;
   std::string temps;
   std::vector<long> templ1d;
   int pos, ii, j, num, polynum;
   unsigned int i;
   wxClassInfo * cls;
   wxRect bbox;
   LINK * ln;
   POLY tmpPoly;
   std::map<int, MODULE>::iterator iter;
   //Read it from the file
   int modsize = 0;
   MODULE temp_mod;

   while (s_mutexProtect.Lock()!=wxMUTEX_NO_ERROR){;}

   for (i=0; i< links.size(); i++)
   {
      delete links[i];
   }
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
      else if ((pos=vars[i].find("ln_FrMod_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+9, 4).c_str());
	      links[num]->Fr_mod=temp;
	   }
      else if ((pos=vars[i].find("ln_ToMod_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+9, 4).c_str());
	      links[num]->To_mod=temp;
	   }
      else if ((pos=vars[i].find("ln_FrPort_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      links[num]->Fr_port=temp;
	   }
      else if ((pos=vars[i].find("ln_ToPort_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      links[num]->To_port=temp;
	   }
      else if ((pos=vars[i].find("tag_Con0X_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      tags[num].cons[0].x = temp;
	   }
      else if ((pos=vars[i].find("tag_Con0Y_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      tags[num].cons[0].y = temp;
	   }
      else if ((pos=vars[i].find("tag_Con1X_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      tags[num].cons[1].x = temp;
	   }
      else if ((pos=vars[i].find("tag_Con1Y_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+10, 4).c_str());
	      tags[num].cons[1].y = temp;
	   }
      else if ((pos=vars[i].find("tag_BoxX_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+9, 4).c_str());
	      tags[num].box.x = temp;
	   }
      else if ((pos=vars[i].find("tag_BoxY_"))!=(int)std::string::npos)
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
      if ((pos=vars[i].find("modCls_"))!=(int)std::string::npos)
	   {
	      num =atoi(vars[i].substr(pos+7, 4).c_str());
	      cls = wxClassInfo::FindClass(temps.c_str());
	      if (cls==NULL)
	      {
	         // wxMessageBox("Load failed : You don't have that class in your Plugin DLL!", temps.c_str());
	         for (ii=0; ii< (int)links.size(); ii++)
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
         modules[num].pl_mod->SetID(num);
	      modules[num].cls_name = temps;
	   }
   
      if ((pos=vars[i].find("tag_Txt_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+8, 4).c_str());
	      tags[num].text = wxString(temps.c_str());
	   }
   }
   
   vars = ntpk.getInts1D();
   for (i=0; i<vars.size(); i++)
   {
      ntpk.getVal(vars[i],templ1d);
      if ((pos=vars[i].find("ln_ConX_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+8, 4).c_str());

	      if (links[num]->cons.size()==0)
	         links[num]->cons.resize(templ1d.size());

	      for (j=0; j<(int)templ1d.size(); j++)
	         links[num]->cons[j].x = templ1d[j];
	   }
      else if ((pos=vars[i].find("ln_ConY_"))!=(int)std::string::npos)
	   {
	      num = atoi(vars[i].substr(pos+8, 4).c_str());
	      for (j=0; j<(int)templ1d.size(); j++)
	         links[num]->cons[j].y = templ1d[j];
	   }
   }

   // unpack the modules' UIs
   // start from 1 because the link interface is the first one
   for(i = 1; i<intfssize; ++i)
   {
      
      _id = intfs[i]._id;
      std::map<int, MODULE >::iterator itr=modules.find(_id);

      if( (intfs[i]._type == 1) && (itr!=modules.end()) )
      {
         modules[_id].pl_mod->UnPack(&intfs[i]);
      }
      else if( intfs[i]._type == 2 )
      {
         Geometry* geometry = new Geometry( modules[_id].pl_mod->GetID() );
         geometry->SetGeometryDataBuffer( modules[_id].pl_mod->GetGeometryDataBuffer() );

         geometry->UnPack(&intfs[i]);

         delete geometry;
      }

   }

   //unpack the Global Param Dialog
   // This is commented out because the computational engine
   // strips the global data out so there is no reason to try
   // to unpack it.
   //globalparam_dlg->UnPack(&intfs[intfs.size()-1]);

   //Now all the data are read from the file. 
   //Let's try to reconstruct the link and the calculate the 
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
  
   while(s_mutexProtect.Unlock()!=wxMUTEX_NO_ERROR){ ; }

   Refresh();
}

