#include "VE_Conductor/Framework/package.h"
#include <xercesc/sax/HandlerBase.hpp>

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
      tmp = 0;
      tmp_attr = 0;
      
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
            bool transparencytoggle = atoi(tmp);
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
            bool colorflag =  atoi(tmp);
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

            /*for(int geomnode=0; geomnode<geomnodenum;geomnode++)
            {
               
              cur_geomelem = (DOMElement*)geomnodelist->item(geomnode);
              infoname = XMLString::transcode(cur_geomelem->getNodeName());
              std::cout<<"[DBG]... the subelement's name is "<<infoname<<std::endl;
              if(!strcmp(infoname.c_str(), "ModelType"))
              {
                   int modeltype = atoi(XMLString::transcode(cur_geomelem->getAttribute(XMLString::transcode("val"))));
                   geomvals.SetModelType(modeltype);
                   break;

              }
              else if(!strcmp(infoname.c_str(), "GeometryFileName"))
              {
                 std::string geomfilename =std::string(XMLString::transcode(cur_geomelem->getAttribute(XMLString::transcode("val"))));
                 std::cout<<"[DBG]...geometry file name is "<<geomfilename<<std::endl;
                 geomvals.SetGeomFileName(geomfilename);  
                 break;

              }
              else if(!strcmp(infoname.c_str(), "TransparencyToggle"))
              {
                 bool transparencytoggle =  atoi(XMLString::transcode(cur_geomelem->getAttribute(XMLString::transcode("val"))));
                 geomvals.SetTransparencyToggle(transparencytoggle);  
                 break;

              }
              else if(!strcmp(infoname.c_str(), "ScaleArray"))
              {
                 double scales[3];
                  for(int index=0; index<3; index++)
                  {
                     domt = (DOMText*) sub_list->item(index)->getFirstChild();
                     scales[i]=(double) atof(XMLString::transcode(domt->getData()));
                  }
                  geomvals.SetScales(scales[0], scales[1], scales[1]);
                  break;

              }
              else if(!strcmp(infoname.c_str(), "TranslationArray"))
              {
                  double trans[3];
                  for(int index=0; index<3; index++)
                  {
                     domt = (DOMText*) sub_list->item(index)->getFirstChild();
                     trans[i]=(double) atof(XMLString::transcode(domt->getData()));
                  }
                  geomvals.SetTrans(trans[0], trans[1], trans[2]);
                  break;

              }
              else if(!strcmp(infoname.c_str(), "RotationArray"))
              {
                 double rots[3];
                  for(int index=0; index<3; index++)
                  {
                     domt = (DOMText*) sub_list->item(index)->getFirstChild();
                     rots[i]=(double) atof(XMLString::transcode(domt->getData()));
                  }
                  geomvals.SetRots(rots[0], rots[1],rots[2]);
                  break;
              }
              else if(!strcmp(infoname.c_str(), "ColorFlag"))
              {
                  bool colorflag = atoi(XMLString::transcode(cur_geomelem->getAttribute(XMLString::transcode("val"))));
                  geomvals.SetColorFlag(colorflag);
                  break;

              }
              else if(!strcmp(infoname.c_str(), "RGBMatrix"))
              {
                 double colors[3];
                  for(int index=0; index<3; index++)
                  {
                     domt = (DOMText*) sub_list->item(index)->getFirstChild();
                     colors[i]=(double) atof(XMLString::transcode(domt->getData()));
                  }
                  geomvals.SetColors(colors[0], colors[1], colors[2]);
                  break;

              }
              else if(!strcmp(infoname.c_str(),"LOD"))
              {
                  double lod =(double) atof(XMLString::transcode(cur_geomelem->getAttribute(XMLString::transcode("val"))));
                  geomvals.SetLOD(lod);
                  break;
              }
              else
              {
                  std::cout<<"Undefine Info"<<std::endl;
                  break;
              }
 

            }*/

	         intfs[i].setGeomInfoPackage(elem_name, geomvals);
       	
         }

      }
         
      else
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
	  /*
	  for(k=0; k<len3; k++)
	    svals.push_back(XMLString::transcode(sub_list->item(k)->getNodeValue()));
		*/
	         intfs[i].setString1D(elem_name, svals);
         }

     
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
  std::vector<std::string> var_names;
  DOMElement* cur_intf;
  DOMElement* cur_elem;
  DOMElement* tmp_elem;
  char tmp[80];
  double dval;
  long lval;
  std::string sval;
  std::vector<double> dvals;
  std::vector<long> lvals;
  std::vector<std::string> svals;
  GeometryInfoPackage geomvals;

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

     //Create nodes for the GeometryInfoPackage
      var_names=intfs[i].getGeomInfoPackages();

      for (j=0; j<var_names.size(); j++)
	{

     
	  cur_elem=doc->createElement(XMLString::transcode("GeomInfo"));

	  cur_elem->setAttribute(XMLString::transcode("name"), XMLString::transcode(var_names[j].c_str()));

     geomvals = intfs[i].getGeomInfoPackage(var_names[j]);
      
     DOMElement* modeltype_elem =doc->createElement(XMLString::transcode("ModelType"));
     DOMElement* geomfilename_elem=doc->createElement(XMLString::transcode("GeometryFileName"));
     DOMElement* transparencytoggle_elem=doc->createElement(XMLString::transcode("TransparencyToggle"));
     DOMElement* scales_elem=doc->createElement(XMLString::transcode("ScaleArray"));
     DOMElement* trans_elem=doc->createElement(XMLString::transcode("TranslationArray"));
     DOMElement* rots_elem=doc->createElement(XMLString::transcode("RotationArray"));
     DOMElement* colorflag_elem=doc->createElement(XMLString::transcode("ColorFlag"));
     DOMElement* colors_elem=doc->createElement(XMLString::transcode("RGBArray"));
     DOMElement* LOD_elem=doc->createElement(XMLString::transcode("LOD"));

     
     sprintf(tmp,"%d", geomvals.GetModelType());
	  modeltype_elem->setAttribute(XMLString::transcode("val"), XMLString::transcode(tmp));
	  
     
     sprintf(tmp,"%s", geomvals.GetGeomFileName().c_str());
     geomfilename_elem->setAttribute(XMLString::transcode("val"),XMLString::transcode(tmp));
     
     
     sprintf(tmp,"%d", geomvals.GetTransparencyToggle());
     transparencytoggle_elem->setAttribute(XMLString::transcode("val"),XMLString::transcode(tmp));
     
     double* temp;
     temp = geomvals.GetScales();
     
      for (k=0; k<3; k++)
	    {
	      sprintf(tmp,"%0.10g", temp[k]);
	      tmp_elem=doc->createElement(XMLString::transcode("val"));
	      tmp_elem->appendChild(doc->createTextNode(XMLString::transcode(tmp)));
	      scales_elem->appendChild(tmp_elem);
   
	    }

      temp = geomvals.GetTrans();
       for (k=0; k<3; k++)
	    {
	      sprintf(tmp,"%0.10g", temp[k]);
	      tmp_elem=doc->createElement(XMLString::transcode("val"));
	      tmp_elem->appendChild(doc->createTextNode(XMLString::transcode(tmp)));
	      trans_elem->appendChild(tmp_elem);
	    }

       temp = geomvals.GetRots();
       for (k=0; k<3; k++)
	    {
	      sprintf(tmp,"%0.10g", temp[k]);
	      tmp_elem=doc->createElement(XMLString::transcode("val"));
	      tmp_elem->appendChild(doc->createTextNode(XMLString::transcode(tmp)));
	      rots_elem->appendChild(tmp_elem);
	    }

       temp = geomvals.GetColors();
      for (k=0; k<3; k++)
	    {
	      sprintf(tmp,"%0.10g", temp[k]);
	      tmp_elem=doc->createElement(XMLString::transcode("val"));
	      tmp_elem->appendChild(doc->createTextNode(XMLString::transcode(tmp)));
	      colors_elem->appendChild(tmp_elem);
	    }

      
     sprintf(tmp,"%d", geomvals.GetColorFlag());
     colorflag_elem->setAttribute(XMLString::transcode("val"),XMLString::transcode(tmp));
     
     sprintf(tmp, "%0.10g", geomvals.GetLOD());
     LOD_elem->setAttribute(XMLString::transcode("val"), XMLString::transcode(tmp));

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
 

    
    }
  
  return doc;

}
