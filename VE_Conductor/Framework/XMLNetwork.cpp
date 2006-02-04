//////////////////////////////////////////////
//////// Save and Load Functions /////////////
//////////////////////////////////////////////
void Network::Save( DOMDocument* doc )
{
   // Here we wshould loop over all of the following
   //  Newtork
   if ( veNetwork )
      delete veNetwork;
   
   veNetwork = new VE_Model::Network();

   veNetwork->GetDataValuePair( -1 )->SetData( "m_xUserScale", userScale.first );
   veNetwork->GetDataValuePair( -1 )->SetData( "m_yUserScale", userScale.second );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixX", numPix.first );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixY", numPix.second );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitX", numUnit.first );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitY", numUnit.second );

   for ( size_t i = 0; i < links.size(); ++i )
   {
      VE_Model::Link* xmlLink = veNetwork->GetLink( -1 );
      xmlLink->GetFromPort()->SetData( modules[ links[i]->GetFromModule() ].GetPlugin()->GetModelName(), links[i]->GetFromPort() );
      xmlLink->GetToPort()->SetData( modules[ links[i]->GetToModule() ].pl_mod->GetModelName(), links[i]->GetToPort() );

      //Try to store link cons,
      //link cons are (x,y) wxpoint
      //here I store x in one vector and y in the other
      for ( size_t j = 0; j < links[ i ].GetNumberOfPoints(); ++j )
	   {
         xmlLink->GetLinkPoint( j )->SetPoint( std::pair< unsigned int, unsigned int >( links[ i ]->GetPoint( j )->x, links[ i ]->GetPoint( j )->y );
      }
   }

   doc->getDocumentElement()->appendChild
         (
            veNetwork->GetXMLData( "veNetwork" );
         );


   //  Models
   std::map< int, Module >::iterator iter;
   for ( iter=modules.begin(); iter!=modules.end(); ++iter )
   {
      modules[ iter->first ].GetPlugin()->SetID(i);
      doc->getDocumentElement()->appendChild
         ( 
            modules[ iter->first ].GetPlugin()->GetVEModel()->GetXMLData( "veModel" )
         );
   }

   //  tags
   /*for ( size_t i = 0; i < veTagVector.size(); ++i )
   {
      delete veTagVector.at( i );
   }
   veTagVector.clear();

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
   }*/
}

////////////////////////////////////////////////////////
void Network::Load( DOMDocument* doc )
{
   // Load from the nt file loaded through wx
   // Get a list of all the command elements

   // do this for network
   if ( veNetwork )
      delete veNetwork;
   
   veNetwork = new VE_Model::Network( doc );

   veNetwork->SetObjectFromXMLData
               ( 
                  dynamic_cast< DOMElement* >( doc->getDocumentElement()
                                                   ->getElementsByTagName( xercesString("veNetwork") )
                                                   ->item( 0 ) ) 
               );
   veNetwork->GetDataValuePair( 0 )->GetData( &m_xUserScale );
   veNetwork->GetDataValuePair( 1 )->GetData( &m_yUserScale );
   veNetwork->GetDataValuePair( 2 )->GetData( &nPixX );
   veNetwork->GetDataValuePair( 3 )->GetData( &nPixY );
   veNetwork->GetDataValuePair( 4 )->GetData( &nUnitX );
   veNetwork->GetDataValuePair( 5 )->GetData( &nUnitX );

   for ( size_t i = 0; i < links.size(); ++i )
   {
      delete links.at( i )
   }
   links.clear();

   for ( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++i )
   {
	   links.push_back( new LINK() );
      veNetwork->GetLink( i )->GetFromPort()->GetData( "ln_FrMod_", &(links.at( i )->Fr_mod) );
      veNetwork->GetLink( i )->GetFromPort()->GetData( "ln_ToMod_", &(links.at( i )->To_mod) );
      veNetwork->GetLink( i )->GetFromPort()->GetData( "ln_FrPort_", &(links.at( i )->Fr_port) );
      veNetwork->GetLink( i )->GetFromPort()->GetData( "ln_ToPort_", &(links.at( i )->To_port) );
      size_t numberOfPoints = veNetwork->GetLink( i )->GetNumberOfLinkPoints();
      for ( size_t j = 0; j < numberOfPoints; ++j )
      {
         std::pair< unsigned int, unsigned int > rawPoint = veNetwork->GetLink( i )->GetLinkPoint()->GetPoint();
         wxPoint point;
         point.x = rawPoint.first;
         point.y = rawPoint.second;
         links.at( i )->cons.push_back( point );
      }
   }

   // do this for models
   DOMNodeList* subElements = doc->getDocumentElement()->getElementsByTagName( xercesString("veModel") );
   unsigned int numModels = subElements->getLength();
   // now lets create a list of them
   for ( unsigned int i = 0; i < numModels; ++i )
   {
         VE_Model::Model* model = new VE_Model::Model( doc );
         model->SetObjectFromXMLData( dynamic_cast< DOMElement* >( subElements->item( i ) ) );

         wxClassInfo* cls = wxClassInfo::FindClass( model->GetModelName().c_str() );
         REI_Plugin* tempPlugin = dynamic_cast< REI_Plugin* >( cls->CreateObject() );
         MODULE temp_mod;
	      modules[ model->GetModelID() ] = temp_mod;
	      modules[num].pl_mod = tempPlugin;
         modules[num].pl_mod->SetID( model->GetModelID() );
	      modules[num].cls_name = model->GetModelName();
         *(modules[num].pl_mod->GetModel()) = *model;
         delete model;
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
