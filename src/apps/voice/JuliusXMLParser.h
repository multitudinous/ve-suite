
#ifndef VE_VOICE_JULIUS_XML_PARSER_H_
#define VE_VOICE_JULIUS_XML_PARSER_H_

#include "apps/voice/SpeechRecognitionObserver.h"

#include <loki/SmartPtr.h>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#include <xercesc/parsers/SAX2XMLReaderImpl.hpp>

#include <string>
#include <vector>

/**
 * Parses blocks of XML that are returned from a Julius server.
 * Julius has its own set of XML tags that represent results and state
 * changes from the speech recognition engine.
 */
class JuliusXMLParser
{
public:

    JuliusXMLParser();

    ~JuliusXMLParser();

    /**
     * Attaches the given observer to this Subject.
     *
     * @param   observer    the observer to attach. 
     */
    void attach(const SpeechRecognitionObserverPtr& observer)
    {
      mObservers.push_back(observer);
    }

    /**
     * Detaches the given observer from this Subject; the observer will no
     * longer be notified when new events occur.
     *
     * @param   observer    the observer to detach.
     */
    void detach(const SpeechRecognitionObserverPtr& observer);

    /**
     * Parses the given XML text and notifies observers of the results.
     *
     * @param   text        the text to parse.
     *
     * @return     true if successful, false otherwise.
     */
    bool parse(const std::string& text);

private:

    /// The list of observers that are attached to this subject.
    std::vector<SpeechRecognitionObserverPtr>          mObservers;

    /// The SAX parser used to parse the XML.
    XERCES_CPP_NAMESPACE_QUALIFIER SAX2XMLReader*      mParser;
    
    /* Private Functions */

    /**
     * This function handles a start element event from SAX.
     */
    void startElement(const XMLCh* const uri, const XMLCh* const localname,
                      const XMLCh* const qname, 
                      const XERCES_CPP_NAMESPACE_QUALIFIER Attributes& attrs);
};

typedef Loki::SmartPtrDef<JuliusXMLParser>::type JuliusXMLParserPtr;

#endif
// vim:ts=4:sw=4:et:tw=0
