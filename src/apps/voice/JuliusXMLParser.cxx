
#include "apps/voice/JuliusXMLParser.h"

#include <loki/Typelist.h>
#include <loki/TypelistMacros.h>
#include <loki/Functor.h>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#include <xercesc/parsers/SAX2XMLReaderImpl.hpp>

// XXX:  Remove this!
#include <iostream>

namespace 
{
   XERCES_CPP_NAMESPACE_USE;
   /**
    * SAX requires the use of callbacks to handle XML Parsing.
    * Since SAX requires the callbacks to inherit from their Handler
    * hierarchy, the handler classes were placed in an anonymous namespace
    * to keep from cluttering the voice tree with SAX specific XML
    * parsing classes.
    */
   class JuliusContentHandler : public DefaultHandler
   {
   public:

      // Typedef the Functor signatures for each of the different event
      // types that SAX will notify us of.
      typedef Loki::Functor<void, LOKI_TYPELIST_2(const XMLCh* const, 
                                                  const unsigned int)>
              CharacterHandler;
      typedef Loki::Functor<void>
              EndDocumentHandler;
      typedef Loki::Functor<void, LOKI_TYPELIST_3(const XMLCh* const,
                                                  const XMLCh* const,
                                                  const XMLCh* const)>
              EndElementHandler;
      typedef Loki::Functor<void, LOKI_TYPELIST_4(const XMLCh* const,
                                                  const XMLCh* const,
                                                  const XMLCh* const,
                                                  const Attributes&)>
              StartElementHandler;
      typedef Loki::Functor<void, LOKI_TYPELIST_1(const XMLCh* const)>
              SkippedEntityHandler;

      JuliusContentHandler()
      {}

      ~JuliusContentHandler()
      {}

      void setCharacterHandler(const CharacterHandler& handler)
      {
         mCharacterHandler = handler;
      }

      void setEndDocumentHandler(const EndDocumentHandler& handler)
      {
         mEndDocumentHandler = handler;
      }

      void setEndElementHandler(const EndElementHandler& handler)
      {
         mEndElementHandler = handler;
      }

      void setStartElementHandler(const StartElementHandler& handler)
      {
         mStartElementHandler = handler;
      }

      void setSkippedEntityHandler(const SkippedEntityHandler& handler)
      {
         mSkippedEntityHandler = handler;
      }

      // TODO:  Remove the debugging handlers once the SAX event hierarchy
      //        is better understood.

      void characters(const XMLCh* const chars, const unsigned int length)
      {
         std::cout << "[DBG] Characters Event Size: " << length
                   << "Chars '" 
                   << XMLString::transcode(chars) << "'." << std::endl;
         if (!mCharacterHandler.empty())
         {
            mCharacterHandler(chars, length);
         }
      }

      void endDocument()
      {
         std::cout << "[DBG] End document" << std::endl;
         if (!mEndDocumentHandler.empty())
         {
            mEndDocumentHandler();
         }
      }

      void endElement(const XMLCh* const uri, const XMLCh* const localname,
                      const XMLCh* const qname)
      {
         std::cout << "[DBG] End Element URI: '" << XMLString::transcode(uri)
                   << "' localname: '" << XMLString::transcode(localname)
                   << "' qname: '" << XMLString::transcode(qname)
                   << "'." << std::endl;

         if (!mEndElementHandler.empty())
         {
            mEndElementHandler(uri, localname, qname);
         }
      }

      void startElement(const XMLCh* const uri, const XMLCh* const localname,
                        const XMLCh* const qname, const Attributes& attrs)
      {
         if (!mStartElementHandler.empty())
         {
            std::cout << "[DBG] Invoking Functor." << std::endl;
            mStartElementHandler(uri, localname, qname, attrs);
         }
         else
         {
            std::cout << "[DBG] Functor is empty." << std::endl;
         }
         /*
         std::cout << "[DBG] Start Element: URI: '" 
                   << XMLString::transcode(uri)
                   << "' localname: '" << XMLString::transcode(localname)
                   << "' qname: '" << XMLString::transcode(qname) << "'." 
                   << std::endl;
         std::cout << "[DBG] There are " << attrs.getLength() << " attributes."
                   << std::endl;
         for (size_t i = 0; i < attrs.getLength(); ++i)
         {
            std::cout << "[DBG] Attribute " << i << " "
                      << "Qname: " << XMLString::transcode(attrs.getQName(i))
                      << " "
                      << "URI: " << XMLString::transcode(attrs.getURI(i)) 
                      << " "
                      << "local: "<< XMLString::transcode(attrs.getLocalName(i))
                      << " "
                      << "type: " << XMLString::transcode(attrs.getType(i)) 
                      << " "
                      << "value: " << XMLString::transcode(attrs.getValue(i)) 
                      << std::endl;
         }
         */
      }

      void skippedEntity(const XMLCh* const name)
      {
         std::cout << "[DBG] Skipped Entity: '" 
                   << XMLString::transcode(name) << "'." << std::endl;
         if (!mSkippedEntityHandler.empty())
         {
            mSkippedEntityHandler(name);
         }
      }

   private:

      /// The functor to invoke when a characters event is received.
      CharacterHandler                           mCharacterHandler;

      /// The functor to invoke when an End Document event is received.
      EndDocumentHandler                         mEndDocumentHandler;

      /// The functor to invoke when an End Element event is received.
      EndElementHandler                          mEndElementHandler;

      /// The functor to invoke when a Start Element event is received.
      StartElementHandler                        mStartElementHandler;

      /// The functor to invoke when a Skipped element event is received.
      SkippedEntityHandler                      mSkippedEntityHandler;

   };

   static JuliusContentHandler gHandler;
}

JuliusXMLParser::JuliusXMLParser()
   : mParser(NULL)
{
   /// This is required by the Xerces-C++ API.
   XERCES_CPP_NAMESPACE_USE;
   XMLPlatformUtils::Initialize();
   mParser = new SAX2XMLReaderImpl();
   mParser->setContentHandler(&gHandler);
   mParser->setDTDHandler(&gHandler);
   mParser->setEntityResolver(&gHandler);
   mParser->setErrorHandler(&gHandler);
   JuliusContentHandler::StartElementHandler sefun(this, 
                         &JuliusXMLParser::startElement);
   gHandler.setStartElementHandler(sefun);
}

JuliusXMLParser::~JuliusXMLParser()
{
   /// This is required by the Xerces-C++ API.
   XERCES_CPP_NAMESPACE_USE;
   delete mParser;
   XMLPlatformUtils::Terminate();
}

void
JuliusXMLParser::detach(const SpeechRecognitionObserverPtr& observer)
{
   // Linear search should be okay here since there shouldn't ever be
   // a large number of observers.
   std::vector<SpeechRecognitionObserverPtr>::iterator itr;
   for (itr = mObservers.begin(); itr != mObservers.end(); ++itr)
   {
      if ((*itr) == observer)
      {
         mObservers.erase(itr);
      }
   }
}

bool
JuliusXMLParser::parse(const std::string& text)
{
   XERCES_CPP_NAMESPACE_USE;
   std::cout << "[DBG] I got: \"" << text << "\"" << std::endl;

   size_t size = text.size() + 1;
   // Create an input source from the text string.
   MemBufInputSource* mem_buf_is = new MemBufInputSource(
                                reinterpret_cast<const XMLByte*>(text.c_str()),
                                size, "Julius XML Text", false );

   try
   {
      mParser->parse(*mem_buf_is);
   }
   catch (const XMLException& e)
   {
      std::cerr << "\n[ERR] Exception thrown while parsing: "
                << XMLString::transcode(e.getMessage()) << std::endl;
   }
   catch (const SAXParseException& e)
   {
      std::cerr << "\n[ERR] Exception thrown while parsing: "
                << "Line: " << e.getLineNumber()
                << "Column: " << e.getColumnNumber()
                << "Error: '"
                << XMLString::transcode(e.getMessage()) << "'." << std::endl;
   }

   delete mem_buf_is;
   return true;
}

void
JuliusXMLParser::startElement(const XMLCh* const uri, 
                              const XMLCh* const localname,
                              const XMLCh* const qname, const Attributes& attrs)
{
   std::cout << "[DBG] In Functor." << std::endl;
   static bool phrase_begun = false;
   static std::string phrase = "";
   static std::string space = "";

   XMLCh* whypo = XMLString::transcode("WHYPO");
   XMLCh* word_tag = XMLString::transcode("WORD");
   XMLCh* recogfail = XMLString::transcode("RECOGFAIL");
   if (0 == XMLString::compareIString(qname, whypo))
   {
      const XMLCh* word_value = attrs.getValue(word_tag);
      if (!word_value)
      {
         std::cout << "[ERR] WHYPO had no word!" << std::endl;
      }
      else
      {
         char* word = XMLString::transcode(word_value);
         std::string word_str = word;
         std::cout << "[DBG] Recognized: " << word_str << std::endl;
         if ("<s>" == word_str)
         {
            phrase_begun = true;
         }
         else if ("</s>" == word_str)
         {
            phrase_begun = false;
            std::vector<SpeechRecognitionObserverPtr>::iterator itr;
            for (itr = mObservers.begin(); itr != mObservers.end(); ++itr)
            {
               (*itr)->onPhraseRecognition(phrase);
            }
            phrase = "";
            space = "";
         }
         else
         {
            phrase += space + word_str;
            space = " ";
         }
         XMLString::release(&word);
      }
   }
   else if (0 == XMLString::compareIString(qname, recogfail))
   {
      std::vector<SpeechRecognitionObserverPtr>::iterator itr;
      for (itr = mObservers.begin(); itr != mObservers.end(); ++itr)
      {
         (*itr)->onFailedRecognition();
      }
   }
   else
   {
      std::cout << "[DBG] Not a WHYPO." << std::endl;
   }
   XMLString::release(&whypo);
   XMLString::release(&word_tag);
   XMLString::release(&recogfail);
   
   /*std::cout << "[DBG] Start Element: URI: '" 
             << XMLString::transcode(uri)
             << "' localname: '" << XMLString::transcode(localname)
             << "' qname: '" << XMLString::transcode(qname) << "'." 
             << std::endl;
   std::cout << "[DBG] There are " << attrs.getLength() << " attributes."
             << std::endl;
   for (size_t i = 0; i < attrs.getLength(); ++i)
   {
      std::cout << "[DBG] Attribute " << i << " "
                << "Qname: " << XMLString::transcode(attrs.getQName(i))
                << " "
                << "URI: " << XMLString::transcode(attrs.getURI(i)) 
                << " "
                << "local: "<< XMLString::transcode(attrs.getLocalName(i))
                << " "
                << "type: " << XMLString::transcode(attrs.getType(i)) 
                << " "
                << "value: " << XMLString::transcode(attrs.getValue(i)) 
                << std::endl;
   }*/
}
