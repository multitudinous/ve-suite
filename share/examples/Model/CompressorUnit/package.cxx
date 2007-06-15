/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "package.h"

Package::Package() 
{
  packname="Package";
  system_id="pack.xml";
  
  intfs.clear();
 

}

Package::~Package()
{
  intfs.clear();
}

bool Package::Load()
{
  char * message;

  //  cout<<system_id<<endl;
  XercesDOMParser* parser = new XercesDOMParser;
  ErrorHandler* errHandler ;
  
  errHandler = (ErrorHandler*) new HandlerBase();
 
  if (parser==NULL)
    cout<<"NULL parser"<<endl;
  parser->setValidationScheme(XercesDOMParser::Val_Always);    // optional.
  parser->setDoNamespaces(true);    // optional
  
  parser->setErrorHandler(errHandler);
  try {
    parser->parse(XMLString::transcode(system_id.c_str()));
  }
  catch (const XMLException& toCatch) {
    message = XMLString::transcode(toCatch.getMessage());
    cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    delete parser;
    delete errHandler;

    return false;
  }
  catch (const DOMException& toCatch) {
    message = XMLString::transcode(toCatch.msg);
    cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    delete parser;
    delete errHandler;

    return false;
  }
  catch (...) {
    cout << "Unexpected Exception \n" ;
    delete parser;
    delete errHandler;

    return false;
  }


  DOMDocument *doc = parser->getDocument(); //This is the rootNode;
  
  if (doc == NULL)
    cout<<"NULL document!"<<endl;
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
    cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    delete parser;
    delete errHandler;

    return false;
  }
  catch (const DOMException& toCatch) {
    message = XMLString::transcode(toCatch.msg);
    cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    delete parser;
    delete errHandler;

    return false;
  }
  catch (...) {
    cout << "Unexpected Exception \n" ;
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
    cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    return false;
  }
  catch (const DOMException& toCatch) {
    message = XMLString::transcode(toCatch.msg);
    cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    return false;
  }
  catch (...) {
    cout << "Unexpected Exception \n" ;
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
    cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    rv=false;
    return NULL;
  }
  catch (const DOMException& toCatch) {
    message = XMLString::transcode(toCatch.msg);
    cout << "Exception message is: \n"
	 << message << "\n";
    XMLString::release(&message);
    rv=false;
    return NULL;
  }
  catch (...) {
    cout << "Unexpected Exception \n" ;
    rv=false;
    return NULL;
  }

  theSerializer->release();
  rv=true;
  return result; 
}

void Package::FillIntfs(DOMDocument *doc)
{
  DOMElement *root_elem = doc->getDocumentElement(); //This is the root element;

  packname=XMLString::transcode(root_elem->getAttribute(XMLString::transcode("name")));

  DOMNodeList* list_intfs = root_elem->getElementsByTagName(XMLString::transcode("interface")); //Here, all interface element are extracted to this list
 
  int len = list_intfs->getLength();

  int i, j, k, len2, len3;
  DOMElement *cur_intf;
  DOMNodeList* cur_list;
  DOMNodeList* sub_list;
  DOMElement *tmp_elem;
  string elem_name;
  double dval;
  long lval;
  string sval;
  vector<double> dvals;
  vector<long> lvals;
  vector<string> svals;
  DOMText *domt;

  intfs.clear();
  intfs.resize(len);

  for (i=0; i<len; i++)
    {
      cur_intf=(DOMElement *)list_intfs->item(i);
      intfs[i].clear();

      intfs[i]._type = atoi(XMLString::transcode(cur_intf->getAttribute(XMLString::transcode("type"))));
      intfs[i]._category = atoi(XMLString::transcode(cur_intf->getAttribute(XMLString::transcode("category"))));
      intfs[i]._id = atoi(XMLString::transcode(cur_intf->getAttribute(XMLString::transcode("id"))));
      
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
	  sval = string(XMLString::transcode(tmp_elem->getAttribute(XMLString::transcode("val"))));
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
	  /*
	  for(k=0; k<len3; k++)
	  { 
	    const XMLCh * x = sub_list->item(k)->getNodeValue();
		  //lvals.push_back(atoi(XMLString::transcode(sub_list->item(k)->getNodeValue())));

	  }
	  */
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
	  /*
	  for(k=0; k<len3; k++)
	    svals.push_back(XMLString::transcode(sub_list->item(k)->getNodeValue()));
		*/
	  intfs[i].setString1D(elem_name, svals);
       	}
    }  

}

DOMDocument * Package::BuildFromIntfs()
{
  DOMDocument *doc;
  XMLCh tempStr[100];
  
  XMLString::transcode("LS", tempStr, 99);
  DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation(tempStr);
 
  doc =  impl->createDocument(0, XMLString::transcode("package"), 0);
  doc->setVersion(XMLString::transcode("1.0"));
  doc->setEncoding(XMLString::transcode("ISO-8859-1"));
  DOMElement *root_elem = doc->getDocumentElement(); //This is the root element
			      
  root_elem->setAttribute(XMLString::transcode("name"), XMLString::transcode(packname.c_str()));
  root_elem->setAttribute(XMLString::transcode("xmlns:xsi"), XMLString::transcode("http://www.w3.org/2001/XMLSchema-instance"));
  root_elem->setAttribute(XMLString::transcode("xsi:noNamespaceSchemaLocation"), XMLString::transcode("interface.xsd"));
  
  //Now start to create the interfaces

  unsigned int i, j, k;
  vector<string> var_names;
  DOMElement* cur_intf;
  DOMElement* cur_elem;
  DOMElement* tmp_elem;
  char tmp[80];
  double dval;
  long lval;
  string sval;
  vector<double> dvals;
  vector<long> lvals;
  vector<string> svals;

  for (i=0; i<intfs.size(); i++)
    {
      cur_intf=doc->createElement(XMLString::transcode("interface"));
      sprintf(tmp, "%d", intfs[i]._category);
      cur_intf->setAttribute(XMLString::transcode("category"), XMLString::transcode(tmp));
      sprintf(tmp, "%d", intfs[i]._type);
      cur_intf->setAttribute(XMLString::transcode("type"), XMLString::transcode(tmp));
      sprintf(tmp, "%d", intfs[i]._id);
      cur_intf->setAttribute(XMLString::transcode("id"), XMLString::transcode(tmp));
      root_elem->appendChild(cur_intf);

      //Create nodes for the double
      var_names=intfs[i].getDoubles();
      for (j=0; j<var_names.size(); j++)
	{
	  cur_elem=doc->createElement(XMLString::transcode("double"));
	  cur_elem->setAttribute(XMLString::transcode("name"), XMLString::transcode(var_names[j].c_str()));
	  dval = intfs[i].getDouble(var_names[j]);
	  sprintf(tmp,"%.10g", dval);
	  cur_elem->setAttribute(XMLString::transcode("val"), XMLString::transcode(tmp));
	  cur_intf->appendChild(cur_elem);
	}
    
     //Create nodes for the string
      var_names=intfs[i].getStrings();
      for (j=0; j<var_names.size(); j++)
	{
	  cur_elem=doc->createElement(XMLString::transcode("string"));
	  cur_elem->setAttribute(XMLString::transcode("name"), XMLString::transcode(var_names[j].c_str()));
	  sval = intfs[i].getString(var_names[j]);
	  sprintf(tmp,"%s", sval.c_str());
	  cur_elem->setAttribute(XMLString::transcode("val"), XMLString::transcode(tmp));
	  cur_intf->appendChild(cur_elem);
	} 
      
      //Create nodes for the integer
      var_names=intfs[i].getInts();
      for (j=0; j<var_names.size(); j++)
	{
	  cur_elem=doc->createElement(XMLString::transcode("integer"));
	  cur_elem->setAttribute(XMLString::transcode("name"), XMLString::transcode(var_names[j].c_str()));
	  lval = intfs[i].getInt(var_names[j]);
	  sprintf(tmp,"%d", lval);
	  cur_elem->setAttribute(XMLString::transcode("val"), XMLString::transcode(tmp));
	  cur_intf->appendChild(cur_elem);
	} 

      //Create nodes for the doubleArrays
      var_names=intfs[i].getDoubles1D();
      for (j=0; j<var_names.size(); j++)
	{
	  cur_elem=doc->createElement(XMLString::transcode("doubleArray"));
	  cur_elem->setAttribute(XMLString::transcode("name"), XMLString::transcode(var_names[j].c_str()));
	  dvals = intfs[i].getDouble1D(var_names[j]);
	  for (k=0; k<dvals.size(); k++)
	    {
	      sprintf(tmp,"%.10g", dvals[k]);
	      tmp_elem=doc->createElement(XMLString::transcode("val"));
	      tmp_elem->appendChild(doc->createTextNode(XMLString::transcode(tmp)));
	      cur_elem->appendChild(tmp_elem);
	    }
	  cur_intf->appendChild(cur_elem);
	}
	  
      //Create nodes for the string Arrays
      var_names=intfs[i].getStrings1D();
      for (j=0; j<var_names.size(); j++)
	{
	  cur_elem=doc->createElement(XMLString::transcode("stringArray"));
	  cur_elem->setAttribute(XMLString::transcode("name"), XMLString::transcode(var_names[j].c_str()));
	  svals = intfs[i].getString1D(var_names[j]);
	  for (k=0; k<svals.size(); k++)
	    {
	      sprintf(tmp,"%s", svals[k].c_str());
	      tmp_elem=doc->createElement(XMLString::transcode("val"));
	      tmp_elem->appendChild(doc->createTextNode(XMLString::transcode(tmp)));
	      cur_elem->appendChild(tmp_elem);
	    }
	  cur_intf->appendChild(cur_elem);
	}

      //Create nodes for the integer Arrays
      var_names=intfs[i].getInts1D();
      for (j=0; j<var_names.size(); j++)
	{
	  cur_elem=doc->createElement(XMLString::transcode("integerArray"));
	  cur_elem->setAttribute(XMLString::transcode("name"), XMLString::transcode(var_names[j].c_str()));
	  lvals = intfs[i].getInt1D(var_names[j]);
	  for (k=0; k<lvals.size(); k++)
	    {
	      sprintf(tmp,"%d", lvals[k]);
	      tmp_elem=doc->createElement(XMLString::transcode("val"));
	      tmp_elem->appendChild(doc->createTextNode(XMLString::transcode(tmp)));
	      cur_elem->appendChild(tmp_elem);
	    }
	  cur_intf->appendChild(cur_elem);
	}

    }
  
  return doc;

}
