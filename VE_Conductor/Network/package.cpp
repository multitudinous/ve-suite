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
 * File:          $RCSfile: package.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Network/package.h"
#include <xercesc/sax/HandlerBase.hpp>
#include <sstream>
#include <iomanip>
#include <iostream>

Package::Package() 
{
   packname="Package";
   system_id="pack.xml";
   //intfs.clear();
}

Package::~Package()
{
   //intfs.clear();
	/*for ( unsigned int i = 0; i < intfs.size(); ++i )
      delete intfs.at( i );*/

   intfs.clear();
}

std::vector<Interface> Package::GetInterfaceVector( void )
{
	/*std::vector< Interface > retn;
	for ( unsigned int i = 0; i < intfs.size(); ++i )
      retn.push_back( *intfs.at( i ) );
   return retn;*/
   return intfs;
}

bool Package::Load()
{
  char * message;

  //  std::cout<<system_id<<std::endl;
  XercesDOMParser* parser = new XercesDOMParser;
  ErrorHandler* errHandler ;
  
  errHandler = (ErrorHandler*) new HandlerBase();
 
  if (parser==NULL)
    std::cout<<"NULL parser"<<std::endl;
  parser->setValidationScheme(XercesDOMParser::Val_Always);    // optional.
  parser->setDoNamespaces(true);    // optional
  
  parser->setErrorHandler(errHandler);
  try {
    parser->parse(XMLString::transcode(system_id.c_str()));
  }
  catch (const XMLException& toCatch) {
    message = XMLString::transcode(toCatch.getMessage());
    std::cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    delete parser;
    delete errHandler;

    return false;
  }
  catch (const DOMException& toCatch) {
    message = XMLString::transcode(toCatch.msg);
    std::cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    delete parser;
    delete errHandler;

    return false;
  }
  catch (...) {
    std::cout << "Unexpected Exception \n" ;
    delete parser;
    delete errHandler;

    return false;
  }


  DOMDocument *doc = parser->getDocument(); //This is the rootNode;
  
  if (doc == NULL)
    std::cout<<"NULL document!"<<std::endl;
  else
    FillIntfs(doc);

  //... Code to process the DOM nodes
  delete parser;
  delete errHandler;

  return true;

}

bool Package::Load(const char *mem, int len)
{
  MemBufInputSource inpsrc((const XMLByte*)mem, len, system_id.c_str());
  
  char * message;
  XercesDOMParser* parser = new XercesDOMParser();
  ErrorHandler* errHandler;
  
  parser->setValidationScheme(XercesDOMParser::Val_Always);    // optional.
  parser->setDoNamespaces(true);    // optional
  errHandler = (ErrorHandler*) new HandlerBase();
  parser->setErrorHandler(errHandler);

  try {
    parser->parse(inpsrc);
  }
  catch (const XMLException& toCatch) {
    message = XMLString::transcode(toCatch.getMessage());
    std::cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    delete parser;
    delete errHandler;

    return false;
  }
  catch (const DOMException& toCatch) {
    message = XMLString::transcode(toCatch.msg);
    std::cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    delete parser;
    delete errHandler;

    return false;
  }
  catch (...) {
    std::cout << "Unexpected Exception \n" ;
    delete parser;
    delete errHandler;

    return false;
  }

  DOMDocument *doc = parser->getDocument(); //This is the rootNode;

  
  FillIntfs(doc);

  //... Code to process the DOM nodes
  delete parser;
  delete errHandler;

  return true;
			  
}

bool Package::Save()
{
  XMLCh tempStr[100];
  XMLString::transcode("LS", tempStr, 99);
  DOMImplementation *impl = DOMImplementationRegistry::getDOMImplementation(tempStr);
  DOMWriter *theSerializer = ((DOMImplementationLS*)impl)->createDOMWriter();
  theSerializer->setFeature(XMLUni::fgDOMWRTFormatPrettyPrint, true);
  
  XMLFormatTarget *myFormTarget = new LocalFileFormatTarget(XMLString::transcode(system_id.c_str()));
  DOMDocument *doc;
  
  doc=BuildFromIntfs(); //Build the doc tree from the vector of interfaces
  
  char* message;

  try {
    // do the serialization through DOMWriter::writeNode();
    theSerializer->writeNode(myFormTarget, *doc);
  }
  catch (const XMLException& toCatch) {
    message = XMLString::transcode(toCatch.getMessage());
    std::cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    return false;
  }
  catch (const DOMException& toCatch) {
    message = XMLString::transcode(toCatch.msg);
    std::cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    return false;
  }
  catch (...) {
    std::cout << "Unexpected Exception \n" ;
    return false;
  }


  theSerializer->release();
  delete myFormTarget;
  return true; 
}

const char* Package::Save(bool &rv )
{
  XMLCh tempStr[100];
  XMLString::transcode("LS", tempStr, 99);
  DOMImplementation *impl = DOMImplementationRegistry::getDOMImplementation(tempStr);
  DOMWriter *theSerializer = ((DOMImplementationLS*)impl)->createDOMWriter();
  theSerializer->setFeature(XMLUni::fgDOMWRTFormatPrettyPrint, true);
  DOMDocument *doc;

  doc=BuildFromIntfs(); //Build the doc tree from the vector of interfaces
  
  char* message;
  char* result;

  try {
    // do the serialization through DOMWriter::writeNode();
    result=XMLString::transcode(theSerializer->writeToString(*doc));
  }
  catch (const XMLException& toCatch) {
    message = XMLString::transcode(toCatch.getMessage());
    std::cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    rv=false;
    return NULL;
  }
  catch (const DOMException& toCatch) {
    message = XMLString::transcode(toCatch.msg);
    std::cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    rv=false;
    return NULL;
  }
  catch (...) {
    std::cout << "Unexpected Exception \n" ;
    rv=false;
    return NULL;
  }

  theSerializer->release();
  rv=true;
  return result; 
}

void Package::SetIntfsNum(int num)
{
   this->intfsnum = num;
}

int Package::GetIntfsNum()
{
   return this->intfsnum;
}
void Package::FillIntfs(DOMDocument *doc)
{
  DOMElement *root_elem = doc->getDocumentElement(); //This is the root element;

  // Keep track of memory we own.
  XMLCh* tmp_attr( XMLString::transcode("name") );
  char* tmp( XMLString::transcode(root_elem->getAttribute(tmp_attr)) );
  packname = tmp;
  delete[] tmp_attr;
  delete[] tmp;
  tmp_attr = XMLString::transcode("interface");
  DOMNodeList* list_intfs = root_elem->getElementsByTagName(tmp_attr); //Here, all interface element are extracted to this list
  delete[] tmp_attr;
 
  int len = list_intfs->getLength();
  SetIntfsNum(len);
  int i, j, k, len2, len3;
  DOMElement *cur_intf;
  DOMNodeList* cur_list;
  DOMNodeList* sub_list;
  DOMElement *tmp_elem;
  std::string elem_name;
  double dval;
  long lval;
  std::string sval;
  std::vector<double> dvals;
  std::vector<long> lvals;
  std::vector<std::string> svals;
  GeometryInfoPackage geomvals;
  DOMText *domt;

  intfs.clear();
  intfs.resize(len);

  for (i=0; i<len; i++)
    {
       
      cur_intf=(DOMElement *)list_intfs->item(i);
     
      intfs[i].clear();

      // XMLString::transcode() returns a pointer that the caller assumes 
      // ownership of, so we have to keep track of that pointer to properly
      // deallocate the memory after it is used.
      char* tmp(0);
      XMLCh* tmp_attr(0);
      tmp_attr = XMLString::transcode("type");
      tmp = XMLString::transcode(cur_intf->getAttribute(tmp_attr));
      intfs[i]._type = atoi(tmp);
      delete[] tmp;
      delete[] tmp_attr;
      tmp_attr = XMLString::transcode("category");
      tmp = XMLString::transcode(cur_intf->getAttribute(tmp_attr));
      intfs[i]._category = atoi(tmp);
      delete[] tmp;
      delete[] tmp_attr;
      tmp_attr = XMLString::transcode("id");
      tmp = XMLString::transcode(cur_intf->getAttribute(tmp_attr));
      intfs[i]._id = atoi(tmp);
      delete[] tmp;
      delete[] tmp_attr;
      //tmp = 0;
      //tmp_attr = 0;
      if(intfs[i]._type==2 && intfs[i]._category==1) //this is geominfo interface
      {

          //Get the GeomInfoPackages values;
         std::cout<<std::endl;
         tmp_attr = XMLString::transcode("GeomInfo"); 
         cur_list=cur_intf->getElementsByTagName(tmp_attr); //in case one model has several geominfopackage
         delete[] tmp_attr;
         tmp_attr = 0;
         len2=cur_list->getLength();

                  
         DOMNodeList* geomnodelist;
         DOMElement* cur_geomelem;
         std::string infoname;
         
         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *)cur_list->item(j);
            tmp_attr = XMLString::transcode("name");
	         elem_name = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
            geomvals.SetGeomName(elem_name);
            delete[] tmp_attr;
            /*geomnodelist = tmp_elem->getChildNodes();
            int geomnodenum = geomnodelist->getLength();
            std::cout<<"[DBG]...the number of subelements :"<<geomnodenum<<std::endl;*/
            tmp_attr = XMLString::transcode("GeometryFileName");
            geomnodelist = tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            cur_geomelem =(DOMElement*)geomnodelist->item(0);
            tmp_attr = XMLString::transcode("val");
            std::string geomfilename =std::string(XMLString::transcode(cur_geomelem->getAttribute(tmp_attr)));
            delete[] tmp_attr;
            geomvals.SetGeomFileName(geomfilename); 
            tmp_attr = XMLString::transcode("TransparencyToggle"); 
            geomnodelist = tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            cur_geomelem =(DOMElement*)geomnodelist->item(0);
            tmp_attr = XMLString::transcode("val");
            tmp = XMLString::transcode(cur_geomelem->getAttribute(tmp_attr));
			bool transparencytoggle =  (atoi(tmp)== 0)?false:true ;
            delete[] tmp_attr;
            delete[] tmp;
            geomvals.SetTransparencyToggle(transparencytoggle); 

            tmp_attr = XMLString::transcode("ScaleArray");
            geomnodelist = tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            cur_geomelem =(DOMElement*)geomnodelist->item(0);
            tmp_attr = XMLString::transcode("val");
            sub_list=cur_geomelem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            double scales[3];
            for(int index=0; index<3; index++)
            {
               domt = (DOMText*) sub_list->item(index)->getFirstChild();
               tmp = XMLString::transcode(domt->getData());
               scales[index]=(double) atof(tmp);
               delete[] tmp;
            }
            geomvals.SetScales(scales[0], scales[1], scales[2]);

            tmp_attr = XMLString::transcode("TranslationArray");
            geomnodelist = tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            cur_geomelem =(DOMElement*)geomnodelist->item(0);
            tmp_attr = XMLString::transcode("val");
            sub_list=cur_geomelem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            double trans[3];
            for(int index=0; index<3; index++)
            {
               domt = (DOMText*) sub_list->item(index)->getFirstChild();
               tmp = XMLString::transcode(domt->getData());
               trans[index]=(double) atof(tmp);
               delete[] tmp;
            }
            geomvals.SetTrans(trans[0], trans[1], trans[2]);
            tmp_attr = XMLString::transcode("RotationArray");
            geomnodelist = tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            cur_geomelem =(DOMElement*)geomnodelist->item(0);
            tmp_attr = XMLString::transcode("val");
            sub_list=cur_geomelem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            double rots[3];
            for(int index=0; index<3; index++)
            {
               domt = (DOMText*) sub_list->item(index)->getFirstChild();
               tmp = XMLString::transcode(domt->getData());
               rots[index]=(double) atof(tmp);
               delete[] tmp;
            }
            geomvals.SetRots(rots[0], rots[1], rots[2]);

            tmp_attr = XMLString::transcode("RGBArray");
            geomnodelist = tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            cur_geomelem =(DOMElement*)geomnodelist->item(0);
            tmp_attr = XMLString::transcode("val");
            sub_list = cur_geomelem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            double colors[3];
            for(int index=0; index<3; index++)
            {
               domt = (DOMText*) sub_list->item(index)->getFirstChild();
               tmp = XMLString::transcode(domt->getData());
               colors[index]=(double) atof(tmp);
               delete[] tmp;
            }
            geomvals.SetColors(colors[0], colors[1], colors[2]);

            tmp_attr = XMLString::transcode("ColorFlag");
            geomnodelist = tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            cur_geomelem =(DOMElement*)geomnodelist->item(0);
            tmp_attr = XMLString::transcode("val");
            tmp = XMLString::transcode(cur_geomelem->getAttribute(tmp_attr));
            bool colorflag = (atoi(tmp)== 0)?false:true;
            delete[] tmp_attr;
            delete[] tmp;
            geomvals.SetColorFlag(colorflag); 
            
            tmp_attr = XMLString::transcode("LOD");
            geomnodelist = tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
            cur_geomelem =(DOMElement*)geomnodelist->item(0);
            tmp_attr = XMLString::transcode("val");
            tmp = XMLString::transcode(cur_geomelem->getAttribute(tmp_attr));
            double lod =(double) atof(tmp);
            geomvals.SetLOD(lod); 

	         intfs[i].setGeomInfoPackage(elem_name, geomvals);
       	
         }

      }
         
      /*else
      {
         //Get the Double values
         tmp_attr = XMLString::transcode("double");
         cur_list=cur_intf->getElementsByTagName(tmp_attr);
         delete[] tmp_attr;
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *) cur_list->item(j);
            tmp_attr = XMLString::transcode("name");
            tmp = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
            elem_name = tmp;
            delete[] tmp_attr;
            delete[] tmp;
            tmp_attr = XMLString::transcode("val");
            tmp = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
            dval = atof(tmp);
            delete[] tmp_attr;
            delete[] tmp;
            intfs[i].setDouble(elem_name, dval);
         }
      
         //Get the Integer values;
         tmp_attr = XMLString::transcode("integer");
         cur_list=cur_intf->getElementsByTagName(tmp_attr);
         delete[] tmp_attr;
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *) cur_list->item(j);
            tmp_attr = XMLString::transcode("name");
            tmp = XMLString::transcode(tmp_attr);
	         elem_name = tmp;
            delete[] tmp_attr;
            delete[] tmp;
            tmp_attr = XMLString::transcode("val");
            tmp = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
	         lval = atoi(tmp);
            delete[] tmp_attr;
            delete[] tmp;
	         intfs[i].setInt(elem_name, lval);
         }

         //Get the String values;
         tmp_attr = XMLString::transcode("string");
         cur_list=cur_intf->getElementsByTagName(tmp_attr);
         delete[] tmp_attr;
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {  
	         tmp_elem = (DOMElement *)cur_list->item(j);
            tmp_attr = XMLString::transcode("name");
            tmp = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
	         elem_name = tmp;
            delete[] tmp_attr;
            delete[] tmp;
            tmp_attr = XMLString::transcode("val");
            tmp = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
	         sval = std::string(tmp);
            delete[] tmp_attr;
            delete[] tmp;
	         intfs[i].setString(elem_name, sval);
         }
       
         //Get the double array values;
         tmp_attr = XMLString::transcode("doubleArray");
         cur_list=cur_intf->getElementsByTagName(tmp_attr);
         delete[] tmp_attr;
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *)cur_list->item(j);
            tmp_attr = XMLString::transcode("name");
            tmp = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
	         elem_name = tmp;
            delete[] tmp_attr;
            delete[] tmp;
            tmp_attr = XMLString::transcode("val");
	         sub_list=tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
	         len3=sub_list->getLength();
	         dvals.clear();
	         for(k=0; k<len3; k++)
	         {
	            domt = (DOMText*) sub_list->item(k)->getFirstChild();
               tmp = XMLString::transcode(domt->getData());
	            dvals.push_back(atof(tmp));
               delete[] tmp;
	         }
	         intfs[i].setDouble1D(elem_name, dvals);
       	
         }
       
         //Get the integer array values;
         tmp_attr = XMLString::transcode("integarArray");
         cur_list=cur_intf->getElementsByTagName(tmp_attr);
         delete[] tmp_attr;
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *)cur_list->item(j);
            tmp_attr = XMLString::transcode("name");
            tmp = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
	         elem_name = tmp;
            delete[] tmp_attr;
            delete[] tmp;
            tmp_attr = XMLString::transcode("val");
	         sub_list=tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
	         len3=sub_list->getLength();
	         lvals.clear();
	         for(k=0; k<len3; k++)
	         {
	            domt = (DOMText*) sub_list->item(k)->getFirstChild();
               tmp = XMLString::transcode(domt->getData());
	            lvals.push_back(atoi(tmp));
               delete[] tmp;
	         }
	 
            intfs[i].setInt1D(elem_name, lvals);
       	
         }

         //Get the string array values;
         tmp_attr = XMLString::transcode("stringArray");
         cur_list=cur_intf->getElementsByTagName(tmp_attr);
         delete[] tmp_attr;
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *) cur_list->item(j);
            tmp_attr = XMLString::transcode("name");
            tmp = XMLString::transcode(tmp_elem->getAttribute(tmp_attr));
	         elem_name = tmp;
            delete[] tmp_attr;
            delete[] tmp;
            tmp_attr = XMLString::transcode("val");
	         sub_list=tmp_elem->getElementsByTagName(tmp_attr);
            delete[] tmp_attr;
	         len3=sub_list->getLength();
	         svals.clear();
	         for(k=0; k<len3; k++)
	         {
	            domt = (DOMText*) sub_list->item(k)->getFirstChild();
               tmp = XMLString::transcode(domt->getData());
	            svals.push_back(tmp);
               delete[] tmp;
	         }
	
            intfs[i].setString1D(elem_name, svals);
         }

     
      }*/

      else
      {
         //Get the Double values
         cur_list=cur_intf->getElementsByTagName(XMLString::transcode("double"));
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *) cur_list->item(j);
	      elem_name = XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("name")));
	      dval = atof(XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("val"))));
	      intfs[i].setDouble(elem_name, dval);
       	
         }
      
         //Get the Integer values;
         cur_list=cur_intf->getElementsByTagName(XMLString::transcode("integer"));
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *) cur_list->item(j);
	         elem_name = XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("name")));
	         lval = atoi(XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("val"))));
	         intfs[i].setInt(elem_name, lval);
       	
         }

         //Get the String values;
         cur_list=cur_intf->getElementsByTagName(XMLString::transcode("string"));
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {  
	         tmp_elem = (DOMElement *)cur_list->item(j);
	         elem_name = XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("name")));
	         sval = std::string(XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("val"))));
	         intfs[i].setString(elem_name, sval);
       	
         }
       
         //Get the double array values;
         cur_list=cur_intf->getElementsByTagName(XMLString::transcode("doubleArray"));
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *)cur_list->item(j);
	         elem_name = XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("name")));
	         sub_list=tmp_elem->getElementsByTagName(XMLString::transcode("val"));
	         len3=sub_list->getLength();
	         dvals.clear();
	         for(k=0; k<len3; k++)
	         {
	            domt = (DOMText*) sub_list->item(k)->getFirstChild();
	            dvals.push_back(atof(XMLString::transcode(domt->getData())));
	         }
	         intfs[i].setDouble1D(elem_name, dvals);
       	
         }
       
         //Get the integer array values;
         cur_list=cur_intf->getElementsByTagName(XMLString::transcode("integerArray"));
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *)cur_list->item(j);
	         elem_name = XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("name")));
	         sub_list=tmp_elem->getElementsByTagName(XMLString::transcode("val"));
	         len3=sub_list->getLength();
	         lvals.clear();
	         for(k=0; k<len3; k++)
	         {
	            domt = (DOMText*) sub_list->item(k)->getFirstChild();
	            lvals.push_back(atoi(XMLString::transcode(domt->getData())));
	         }
		         intfs[i].setInt1D(elem_name, lvals);
       	
         }

         //Get the string array values;
         cur_list=cur_intf->getElementsByTagName(XMLString::transcode("stringArray"));
         len2=cur_list->getLength();

         for (j=0; j<len2; j++)
	      {
	         tmp_elem = (DOMElement *) cur_list->item(j);
	         elem_name = XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("name")));
	         sub_list=tmp_elem->getElementsByTagName(XMLString::transcode("val"));
	         len3=sub_list->getLength();
	         svals.clear();
	         for(k=0; k<len3; k++)
	         {
	            domt = (DOMText*) sub_list->item(k)->getFirstChild();
	            svals.push_back(XMLString::transcode(domt->getData()));
	         }
		         intfs[i].setString1D(elem_name, svals);
        
         }

     
      }

    
    } 

}

DOMDocument * Package::BuildFromIntfs()
{
  DOMDocument *doc;
  XMLCh tempStr[100];
  XMLCh* transcodeResult(0);
  
  XMLString::transcode("LS", tempStr, 99);
  DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation(tempStr);
 
  transcodeResult = XMLString::transcode("package");
  doc =  impl->createDocument(0, transcodeResult, 0);
  delete[] transcodeResult;
  transcodeResult = XMLString::transcode("1.0");
  doc->setVersion(transcodeResult);
  delete[] transcodeResult;
  transcodeResult = XMLString::transcode("ISO-8859-1");
  doc->setEncoding(transcodeResult);
  delete[] transcodeResult;
  DOMElement *root_elem = doc->getDocumentElement(); //This is the root element
  transcodeResult = XMLString::transcode("name");
  XMLCh* tmp_str( XMLString::transcode(packname.c_str()) );
  root_elem->setAttribute(transcodeResult, tmp_str);
  delete[] transcodeResult;
  delete[] tmp_str;
  transcodeResult = XMLString::transcode("xmlns:xsi");
  tmp_str = XMLString::transcode("http://www.w3.org/2001/XMLSchema-instance");
  root_elem->setAttribute(transcodeResult, tmp_str);
  delete[] transcodeResult;
  delete[] tmp_str;
  transcodeResult = XMLString::transcode("xsi:noNamespaceSchemaLocation");
  tmp_str = XMLString::transcode("interface.xsd");
  root_elem->setAttribute(transcodeResult, tmp_str);
  delete[] transcodeResult;
  delete[] tmp_str;
  
  //Now start to create the interfaces

  unsigned int i, j, k;
  std::vector<std::string> var_names;
  DOMElement* cur_intf;
  DOMElement* cur_elem;
  DOMElement* tmp_elem;
  double dval;
  long lval;
  std::string sval;
  std::vector<double> dvals;
  std::vector<long> lvals;
  std::vector<std::string> svals;
  GeometryInfoPackage geomvals;

   for (i=0; i<intfs.size(); i++)
   {
      XMLCh* tcName( XMLString::transcode("name") );
      XMLCh* tcVal( XMLString::transcode("val") );
      transcodeResult = XMLString::transcode("interface");
      cur_intf=doc->createElement(transcodeResult);
      delete[] transcodeResult;

      std::ostringstream dirStringStream;
      dirStringStream << intfs[i]._category;
      transcodeResult = XMLString::transcode("category");
      XMLCh* tc_tmp = XMLString::transcode( dirStringStream.str().c_str());
      cur_intf->setAttribute(transcodeResult, tc_tmp);
      delete[] transcodeResult;
      delete[] tc_tmp;
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << intfs[i]._type;
      transcodeResult = XMLString::transcode("type");
      tc_tmp = XMLString::transcode( dirStringStream.str().c_str());
      cur_intf->setAttribute(transcodeResult, tc_tmp);
      delete[] transcodeResult;
      delete[] tc_tmp;
      dirStringStream.str("");
      dirStringStream.clear();

      dirStringStream << intfs[i]._id;
      transcodeResult = XMLString::transcode("id");
      tc_tmp = XMLString::transcode( dirStringStream.str().c_str());
      cur_intf->setAttribute(transcodeResult, tc_tmp);
      root_elem->appendChild(cur_intf);
      delete[] transcodeResult;
      delete[] tc_tmp;
      dirStringStream.str("");
      dirStringStream.clear();

      //Create nodes for the double
      var_names=intfs[i].getDoubles();
      for (j=0; j<var_names.size(); j++)
      {
         transcodeResult = XMLString::transcode("double");
         cur_elem=doc->createElement(transcodeResult);
         delete[] transcodeResult;
         transcodeResult = XMLString::transcode(var_names[j].c_str());
         cur_elem->setAttribute(tcName, transcodeResult);
         delete[] transcodeResult;
         dval = intfs[i].getDouble(var_names[j]);

         dirStringStream << std::setprecision(10) << dval;
         tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
         cur_elem->setAttribute(tcVal, tc_tmp);
         delete[] tc_tmp;
         dirStringStream.clear();
         dirStringStream.str("");
         cur_intf->appendChild(cur_elem);
      }
    
     //Create nodes for the string
     var_names=intfs[i].getStrings();
     for (j=0; j<var_names.size(); j++)
     {
         transcodeResult = XMLString::transcode("string");
         cur_elem=doc->createElement(transcodeResult);
         delete[] transcodeResult;
         transcodeResult = XMLString::transcode(var_names[j].c_str());
         cur_elem->setAttribute(tcName, transcodeResult);
         delete[] transcodeResult;
         sval = intfs[i].getString(var_names[j]);

         tc_tmp = XMLString::transcode(sval.c_str());
         cur_elem->setAttribute(tcVal, tc_tmp);
         delete[] tc_tmp;
         cur_intf->appendChild(cur_elem);
     } 
      
     //Create nodes for the integer
     var_names=intfs[i].getInts();
     for (j=0; j<var_names.size(); j++)
     {
         transcodeResult = XMLString::transcode("integer");
         cur_elem=doc->createElement(transcodeResult);
         delete[] transcodeResult;
         transcodeResult = XMLString::transcode(var_names[j].c_str());
         cur_elem->setAttribute(tcName, transcodeResult);
         delete[] transcodeResult;
         lval = intfs[i].getInt(var_names[j]);

         dirStringStream << lval;
         tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
         cur_elem->setAttribute(tcVal, tc_tmp);
         dirStringStream.clear();
         dirStringStream.str("");
         delete[] tc_tmp;
         cur_intf->appendChild(cur_elem);
     } 

     //Create nodes for the doubleArrays
     var_names=intfs[i].getDoubles1D();
     for (j=0; j<var_names.size(); j++)
     {
         transcodeResult = XMLString::transcode("doubleArray");
         cur_elem=doc->createElement(transcodeResult);
         delete[] transcodeResult;
         transcodeResult = XMLString::transcode(var_names[j].c_str());
         cur_elem->setAttribute(tcName, transcodeResult);
         delete[] transcodeResult;
         dvals = intfs[i].getDouble1D(var_names[j]);
	      for (k=0; k<dvals.size(); k++)
         {
            dirStringStream << std::setprecision(10) << dvals[k];
            tmp_elem=doc->createElement(tcVal);
            tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
            tmp_elem->appendChild(doc->createTextNode(tc_tmp));
            delete[] tc_tmp;
            dirStringStream.clear();
            dirStringStream.str("");
            cur_elem->appendChild(tmp_elem);
	      }
	      cur_intf->appendChild(cur_elem);
     }
	  
     //Create nodes for the string Arrays
     var_names=intfs[i].getStrings1D();
     for (j=0; j<var_names.size(); j++)
     {
         transcodeResult = XMLString::transcode("stringArray");
	      cur_elem=doc->createElement(transcodeResult);
         delete[] transcodeResult;
         transcodeResult = XMLString::transcode(var_names[j].c_str());
	      cur_elem->setAttribute(tcName, transcodeResult);
         delete[] transcodeResult;
         svals = intfs[i].getString1D(var_names[j]);
         for (k=0; k<svals.size(); k++)
         {
            //sprintf(tmp,"%s", svals[k].c_str());
            tmp_elem=doc->createElement(tcVal);
            tc_tmp = XMLString::transcode(svals[k].c_str());
            tmp_elem->appendChild(doc->createTextNode(tc_tmp));
            delete[] tc_tmp;
            cur_elem->appendChild(tmp_elem);
         }
         cur_intf->appendChild(cur_elem);
     }

     //Create nodes for the integer Arrays
     var_names=intfs[i].getInts1D();
     for (j=0; j<var_names.size(); j++)
     {
         transcodeResult = XMLString::transcode("integerArray");
         cur_elem=doc->createElement(transcodeResult);
         delete[] transcodeResult;
         transcodeResult = XMLString::transcode(var_names[j].c_str());
         cur_elem->setAttribute(tcName, transcodeResult);
         delete[] transcodeResult;
         lvals = intfs[i].getInt1D(var_names[j]);
         for (k=0; k<lvals.size(); k++)
         {
            dirStringStream << lvals[k];
	         tmp_elem=doc->createElement(tcVal);
            tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
	         tmp_elem->appendChild(doc->createTextNode(tc_tmp));
            delete[] tc_tmp;
            dirStringStream.clear();
            dirStringStream.str("");
            cur_elem->appendChild(tmp_elem);
         }
         cur_intf->appendChild(cur_elem);
      }

   //Create nodes for the GeometryInfoPackage
   var_names=intfs[i].getGeomInfoPackages();

   for (j=0; j<var_names.size(); j++)
   {
      transcodeResult = XMLString::transcode("GeomInfo");
      cur_elem=doc->createElement(transcodeResult);
      delete[] transcodeResult;
      transcodeResult = XMLString::transcode(var_names[j].c_str());
      cur_elem->setAttribute(tcName, transcodeResult);
      delete[] transcodeResult;

      geomvals = intfs[i].getGeomInfoPackage(var_names[j]);
      
      transcodeResult = XMLString::transcode("ModelType");
      DOMElement* modeltype_elem =doc->createElement(transcodeResult);
      delete[] transcodeResult;
      transcodeResult = XMLString::transcode("GeometryFileName");
      DOMElement* geomfilename_elem=doc->createElement(transcodeResult);
      delete[] transcodeResult;
      transcodeResult = XMLString::transcode("TransparencyToggle");
      DOMElement* transparencytoggle_elem=doc->createElement(transcodeResult);
      delete[] transcodeResult;
      transcodeResult = XMLString::transcode("ScaleArray");
      DOMElement* scales_elem=doc->createElement(transcodeResult);
      delete[] transcodeResult;
      transcodeResult = XMLString::transcode("TranslationArray");
      DOMElement* trans_elem=doc->createElement(transcodeResult);
      transcodeResult = XMLString::transcode("RotationArray");
      DOMElement* rots_elem=doc->createElement(transcodeResult);
      delete[] transcodeResult;
      transcodeResult = XMLString::transcode("ColorFlag");
      DOMElement* colorflag_elem=doc->createElement(transcodeResult);
      delete[] transcodeResult;
      transcodeResult = XMLString::transcode("RGBArray");
      DOMElement* colors_elem=doc->createElement(transcodeResult);
      delete[] transcodeResult;
      transcodeResult = XMLString::transcode("LOD");
      DOMElement* LOD_elem=doc->createElement(transcodeResult);
      delete[] transcodeResult;

      dirStringStream << geomvals.GetModelType();
      tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
      modeltype_elem->setAttribute(tcVal, tc_tmp);
      delete[] tc_tmp;
      dirStringStream.str("");
      dirStringStream.clear();

      tc_tmp = XMLString::transcode(geomvals.GetGeomFileName().c_str());
      geomfilename_elem->setAttribute(tcVal, tc_tmp);
      delete[] tc_tmp;

      dirStringStream << geomvals.GetTransparencyToggle();
      tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
      transparencytoggle_elem->setAttribute(tcVal, tc_tmp);
      delete[] tc_tmp;
      dirStringStream.str("");
      dirStringStream.clear();
     
      double* temp;
      temp = geomvals.GetScales();
     
      for (k=0; k<3; k++)
      {
         dirStringStream << std::setprecision(10) << temp[k];
	      tmp_elem=doc->createElement(tcVal);
         tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
	      tmp_elem->appendChild(doc->createTextNode(tc_tmp));
         delete[] tc_tmp;
         dirStringStream.str("");
         dirStringStream.clear();
	      scales_elem->appendChild(tmp_elem);
   
	   }

      temp = geomvals.GetTrans();
      for (k=0; k<3; k++)
	   {
         dirStringStream << std::setprecision(10) << temp[k];
	      tmp_elem=doc->createElement(tcVal);
         tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
	      tmp_elem->appendChild(doc->createTextNode(tc_tmp));
         delete[] tc_tmp;
         dirStringStream.str("");
         dirStringStream.clear();
	      trans_elem->appendChild(tmp_elem);
	   }

      temp = geomvals.GetRots();
      for (k=0; k<3; k++)
	   {
         dirStringStream << std::setprecision(10) << temp[k];
	      tmp_elem=doc->createElement(tcVal);
         tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
	      tmp_elem->appendChild(doc->createTextNode(tc_tmp));
         delete[] tc_tmp;
         dirStringStream.str("");
         dirStringStream.clear();
	      rots_elem->appendChild(tmp_elem);
	   }

      temp = geomvals.GetColors();
      for (k=0; k<3; k++)
	   {
         dirStringStream << std::setprecision(10) << temp[k];
	      tmp_elem=doc->createElement(tcVal);
         tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
	      tmp_elem->appendChild(doc->createTextNode(tc_tmp));
         delete[] tc_tmp;
         dirStringStream.str("");
         dirStringStream.clear();
	      colors_elem->appendChild(tmp_elem);
	   }

      dirStringStream << geomvals.GetColorFlag();
      tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
      colorflag_elem->setAttribute(tcVal, tc_tmp);
      delete[] tc_tmp;
      dirStringStream.str("");
      dirStringStream.clear();
     
      dirStringStream << std::setprecision(10) << geomvals.GetLOD();
      tc_tmp = XMLString::transcode(dirStringStream.str().c_str());
      LOD_elem->setAttribute(tcVal, tc_tmp);
      delete[] tc_tmp;
      dirStringStream.str("");
      dirStringStream.clear();

      cur_elem->appendChild(modeltype_elem);
      cur_elem->appendChild(geomfilename_elem);
      cur_elem->appendChild(transparencytoggle_elem);
      cur_elem->appendChild(scales_elem);
      cur_elem->appendChild(trans_elem);
      cur_elem->appendChild(rots_elem);
      cur_elem->appendChild(colorflag_elem);
      cur_elem->appendChild(colors_elem);
      cur_elem->appendChild(LOD_elem);

     
      cur_intf->appendChild(cur_elem);

	 
   }
   delete[] tcName;
   delete[] tcVal;
 

    
 }
return doc;

}
