
#ifndef VE_JULIUS_XML_PARSER_TEST_H_
#define VE_JULIUS_XML_PARSER_TEST_H_

#include "apps/voice/SpeechRecognitionObserver.h"
#include "apps/voice/JuliusXMLParser.h"

#include <cxxtest/TestSuite.h>

#include <string>
#include <vector>

class JuliusXMLParserTest : public CxxTest::TestSuite
{
public:

   void setUp()
   {
      mTestText += "<JULIUS>\n";
      mTestText += "<GRAMINFO>\n";
      mTestText += "  # 0: [active     ]   23 words,   6 categories,    7 nodes (new) \"sample\"\n";
      mTestText += "</GRAMINFO>\n";
      mTestText += "<GRAMINFO>\n";
      mTestText += "  # 0: [active     ]   23 words,   6 categories,    7 nodes \"sample\"\n";
      mTestText += "  Global:              23 words,   6 categories,    7 nodes\n";
      mTestText += "</GRAMINFO>\n";
      mTestText += "<STARTPROC/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457781\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457784\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457785\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"124\" MSEC=\"1240\"/>\n";
      mTestText += "<RECOGOUT>\n";
      mTestText += "  <SHYPO RANK=\"1\" SCORE=\"-4813.091309\" GRAM=\"0\">\n";
      mTestText += "    <WHYPO WORD=\"&lt;s&gt;\" CLASSID=\"0\" PHONE=\"sil\" CM=\"1.000\"/>\n";
      mTestText += "    <WHYPO WORD=\"CALL\" CLASSID=\"2\" PHONE=\"k ao l\" CM=\"0.950\"/>\n";
      mTestText += "    <WHYPO WORD=\"STEVE\" CLASSID=\"4\" PHONE=\"s t iy v\" CM=\"1.000\"/>\n";
      mTestText += "    <WHYPO WORD=\"&lt;/s&gt;\" CLASSID=\"1\" PHONE=\"sil\" CM=\"1.000\"/>\n";
      mTestText += "  </SHYPO>\n";
      mTestText += "</RECOGOUT>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457785\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457786\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457788\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"138\" MSEC=\"1380\"/>\n";
      mTestText += "<RECOGOUT>\n";
      mTestText += "  <SHYPO RANK=\"1\" SCORE=\"-5231.957031\" GRAM=\"0\">\n";
      mTestText += "    <WHYPO WORD=\"&lt;s&gt;\" CLASSID=\"0\" PHONE=\"sil\" CM=\"1.000\"/>\n";
      mTestText += "    <WHYPO WORD=\"CALL\" CLASSID=\"2\" PHONE=\"k ao l\" CM=\"0.921\"/>\n";
      mTestText += "    <WHYPO WORD=\"JOHNSTON\" CLASSID=\"4\" PHONE=\"jh aa n s t ax n\" CM=\"0.998\"/>\n";
      mTestText += "    <WHYPO WORD=\"&lt;/s&gt;\" CLASSID=\"1\" PHONE=\"sil\" CM=\"1.000\"/>\n";
      mTestText += "  </SHYPO>\n";
      mTestText += "</RECOGOUT>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457788\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457794\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457795\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"72\" MSEC=\"720\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457795\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457797\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457797\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"72\" MSEC=\"720\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457797\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457799\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457800\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"74\" MSEC=\"740\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457800\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457803\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457804\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"138\" MSEC=\"1380\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457804\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457805\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457806\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"74\" MSEC=\"740\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457806\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457807\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457808\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"78\" MSEC=\"780\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457808\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457809\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457810\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"72\" MSEC=\"720\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457810\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457811\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457812\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"68\" MSEC=\"680\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457812\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457813\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457814\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"152\" MSEC=\"1520\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457814\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457815\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457816\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"72\" MSEC=\"720\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457816\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457818\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457819\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"82\" MSEC=\"820\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457819\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457821\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457822\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"82\" MSEC=\"820\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457822\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457823\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457824\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"72\" MSEC=\"720\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457824\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457825\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457826\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"78\" MSEC=\"780\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457826\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457827\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457828\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"78\" MSEC=\"780\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457828\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457829\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457830\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"122\" MSEC=\"1220\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457830\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457831\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457832\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"122\" MSEC=\"1220\"/>\n";
      mTestText += "<RECOGOUT>\n";
      mTestText += "  <SHYPO RANK=\"1\" SCORE=\"-4852.147461\" GRAM=\"0\">\n";
      mTestText += "    <WHYPO WORD=\"&lt;s&gt;\" CLASSID=\"0\" PHONE=\"sil\" CM=\"1.000\"/>\n";
      mTestText += "    <WHYPO WORD=\"CALL\" CLASSID=\"2\" PHONE=\"k ao l\" CM=\"0.926\"/>\n";
      mTestText += "    <WHYPO WORD=\"STEVE\" CLASSID=\"4\" PHONE=\"s t iy v\" CM=\"1.000\"/>\n";
      mTestText += "    <WHYPO WORD=\"&lt;/s&gt;\" CLASSID=\"1\" PHONE=\"sil\" CM=\"1.000\"/>\n";
      mTestText += "  </SHYPO>\n";
      mTestText += "</RECOGOUT>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457832\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457836\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457837\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"82\" MSEC=\"820\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457837\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457839\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457840\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"68\" MSEC=\"680\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457840\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457844\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457845\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"102\" MSEC=\"1020\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457845\"/>\n";
      mTestText += "<INPUT STATUS=\"STARTREC\" TIME=\"1212457847\"/>\n";
      mTestText += "<INPUT STATUS=\"ENDREC\" TIME=\"1212457848\"/>\n";
      mTestText += "<INPUTPARAM FRAMES=\"112\" MSEC=\"1120\"/>\n";
      mTestText += "<RECOGFAIL/>\n";
      mTestText += "<INPUT STATUS=\"LISTEN\" TIME=\"1212457848\"/>\n";
      mTestText += "</JULIUS>\n";
   }

   void tearDown()
   {
      mTestText = "";
      mPhrases.clear();
   }

   void testParse()
   {
      SpeechRecognitionObserverPtr ptr(
            new SpeechRecognitionObserver(
               SpeechRecognitionObserver::PhraseRecognitionHandler(this,
               &JuliusXMLParserTest::onPhraseRecognition),
               SpeechRecognitionObserver::FailedRecognitionHandler(this, 
                  &JuliusXMLParserTest::onFailedRecognition)));
      JuliusXMLParser parser;
      parser.attach(ptr);
      parser.parse(mTestText);
      TS_ASSERT_EQUALS(mPhrases.size(), 23);
      TS_ASSERT_EQUALS(mPhrases[0], "CALL STEVE");
      TS_ASSERT_EQUALS(mPhrases[1], "CALL JOHNSTON");
      TS_ASSERT_EQUALS(mPhrases[2], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[3], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[4], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[5], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[6], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[7], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[8], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[9], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[10], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[11], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[12], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[13], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[14], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[15], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[16], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[17], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[18], "CALL STEVE");
      TS_ASSERT_EQUALS(mPhrases[19], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[20], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[21], "FAIL");
      TS_ASSERT_EQUALS(mPhrases[22], "FAIL");
   }

   void onPhraseRecognition(const std::string& phrase)
   {
      mPhrases.push_back(phrase);
   }

   void onFailedRecognition()
   {
      mPhrases.push_back("FAIL");
   }

private:

   /// Test text
   std::string                                  mTestText;

   /// Phrases that have been recognized.
   std::vector<std::string>                     mPhrases;
   
};

#endif
