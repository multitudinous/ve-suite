#ifndef GETVESUITE_HTRIXIST_HTRIXIST_J12-HT2_H
#define GETVESUITE_HTRIXIST_HTRIXIST_J12-HT2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HTRIXIST_HTRIXIST_J12-HT2( void )
{
    unsigned char osgData[ 5295 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,60,0,142,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,39,246,161,255,0,130,205,126,200,95,183,167,195,255,0,218,155,199,223,12,180,143,141,127,179,203,252,72,255,0,130,75,254,219,31,178,221,167,132,254,53,124,59,180,211,252,81,251,85,252,80,248,241,165,124,62,215,191,100,72,52,203,159,128,62,37,241,190,143,117,225,223,3,157,51,246,138,182,134,243,226,6,175,225,165,240,189,207,237,77,114,222,27,142,234,215,196,126,55,186,211,62,231,178,255,0,131,175,255,0,98,45,70,231,87,179,178,253,145,127,224,161,147,220,232,58,140,90,86,173,18,248,7,246,84,83,101,168,77,165,105,122,220,86,206,210,126,216,32,72,199,75,214,180,201,178,133,151,23,97,75,7,12,171,248,157,251,33,254,203,199,196,31,178,119,236,191,175,127,98,73,47,246,223,236,239,240,83,87,19,11,89,15,155,253,165,240,219,195,87,130,64,68,56,96,198,108,131,147,158,15,122,62,10,126,203,191,218,95,18,127,107,219,33,162,72,255,0,240,143,254,208,254,25,210,54,139,73,72,136,205,251,38,254,204,26,247,148,71,149,242,147,253,181,187,177,253,224,61,8,53,226,127,105,194,21,49,30,234,188,45,125,123,73,71,245,52,80,77,45,79,237,255,0,246,28,253,184,191,103,127,248,40,127,236,231,224,191,218,115,246,103,241,108,190,35,240,31,139,34,91,61,95,67,214,109,237,244,159,31,124,50,241,173,190,157,166,234,90,239,195,63,138,30,23,134,242,224,248,91,199,122,117,190,175,166,75,44,43,61,205,141,253,134,173,167,235,186,29,254,173,225,221,91,72,213,239,254,186,175,243,87,255,0,130,87,254,213,95,182,119,252,19,243,225,239,195,189,87,246,97,189,240,142,187,224,127,143,95,7,190,26,107,62,52,248,65,241,87,72,241,47,138,190,21,222,120,226,15,134,222,19,186,176,248,183,162,104,222,23,241,62,145,127,225,175,137,201,225,205,38,45,18,246,246,199,80,138,219,90,210,146,210,223,95,178,213,36,208,60,43,55,135,255,0,98,255,0,225,248,255,0,240,85,63,250,35,159,177,183,254,27,31,143,92,126,95,31,125,43,208,120,236,52,91,140,167,105,45,26,249,216,149,22,246,63,177,218,43,248,226,255,0,135,227,255,0,193,85,63,232,142,126,198,223,248,108,190,61,127,243,252,163,254,31,143,255,0,5,84,255,0,162,57,251,27,127,225,178,248,245,255,0,207,242,167,251,67,11,255,0,63,63,173,63,204,124,146,63,177,218,43,248,226,255,0,135,227,255,0,193,85,63,232,142,126,198,223,248,108,190,61,127,243,252,163,254,31,143,255,0,5,84,255,0,162,57,251,27,127,225,178,248,245,255,0,207,242,143,237,12,47,252,252,254,180,255,0,49,114,72,254,191,124,89,172,234,62,28,240,175,137,124,67,163,248,79,196,30,61,213,244,47,15,235,58,206,151,224,95,9,220,248,86,207,197,94,52,212,116,189,58,230,250,199,194,126,26,188,241,215,137,116,93,18,215,196,26,141,204,17,89,217,201,172,107,26,78,148,151,55,145,182,161,169,216,90,9,174,162,249,115,246,77,253,182,190,21,254,218,51,252,83,212,254,8,248,123,226,21,239,195,47,134,239,240,69,52,127,140,190,37,210,188,53,225,255,0,3,124,88,111,142,223,179,175,194,207,218,151,195,131,225,222,129,55,139,100,241,125,143,246,119,194,47,141,223,9,238,117,127,248,74,188,47,225,145,29,231,140,227,177,211,191,180,231,176,213,134,159,252,210,255,0,195,241,255,0,224,170,120,63,241,103,63,99,108,255,0,217,49,248,245,239,255,0,85,243,215,21,243,119,236,215,251,126,126,216,31,177,46,131,226,191,135,31,178,151,193,95,133,22,31,9,53,231,248,30,250,94,145,251,66,159,30,252,96,248,141,165,31,129,255,0,178,7,236,213,251,29,105,2,255,0,199,191,13,239,190,30,105,154,186,223,120,51,246,100,240,166,171,115,229,248,98,203,102,167,175,234,34,44,90,253,154,24,87,215,240,220,201,170,171,150,207,239,118,183,234,28,146,63,186,234,43,248,226,255,0,135,227,255,0,193,85,63,232,142,126,198,223,248,108,190,61,127,243,252,163,254,31,143,255,0,5,84,255,0,162,57,251,27,127,225,178,248,245,255,0,207,242,159,246,134,23,254,126,127,90,127,152,249,36,127,99,180,87,241,197,255,0,15,199,255,0,130,170,127,209,28,253,141,191,240,217,124,122,255,0,231,249,71,252,63,31,254,10,169,255,0,68,115,246,54,255,0,195,101,241,235,255,0,159,229,31,218,24,95,249,249,253,105,254,97,201,35,251,29,162,191,142,47,248,126,63,252,21,83,254,136,231,236,109,255,0,134,203,227,215,255,0,63,202,249,147,227,215,252,22,59,254,10,85,226,223,138,159,177,70,191,226,31,133,31,178,125,174,175,240,195,246,155,241,95,142,124,9,6,151,240,239,227,93,189,150,165,226,189,67,246,52,253,173,190,26,94,89,120,142,43,207,141,179,73,125,163,47,130,62,33,248,202,226,56,173,36,180,184,26,133,133,132,205,112,214,176,220,218,220,212,113,216,105,59,42,154,217,191,185,93,254,2,228,103,247,113,69,127,59,31,179,143,252,28,53,240,159,199,223,18,52,79,135,191,181,7,192,61,127,246,83,176,241,142,179,163,232,94,21,248,158,223,19,52,95,138,31,11,116,155,189,66,219,92,107,139,159,139,62,33,187,240,183,134,46,126,28,104,199,82,179,208,108,237,181,8,44,53,171,20,109,110,226,255,0,93,184,240,254,141,166,93,234,117,253,6,120,107,196,190,28,241,167,135,60,63,227,31,7,120,131,68,241,103,132,124,89,162,105,94,37,240,183,138,124,53,170,216,107,190,28,241,47,135,53,219,8,53,77,15,196,30,31,215,52,185,229,181,214,116,75,205,50,234,214,230,210,238,218,89,32,184,130,230,57,161,145,227,117,99,209,10,148,234,174,106,114,82,94,66,105,173,207,231,107,254,9,191,240,37,245,143,248,39,135,236,21,171,127,103,171,141,83,246,47,253,151,53,13,248,140,239,23,191,3,188,13,114,91,147,156,17,40,52,126,203,159,2,94,251,227,135,252,20,130,216,88,6,254,200,253,180,60,13,167,16,86,51,229,249,191,240,78,255,0,216,43,86,11,201,244,212,193,227,251,230,190,172,255,0,130,94,124,77,208,52,255,0,248,38,127,252,19,186,194,97,110,102,178,253,133,255,0,100,155,73,73,141,139,121,182,223,0,126,31,197,33,36,39,222,220,141,71,236,147,241,55,195,246,223,31,255,0,224,168,83,72,182,219,111,255,0,110,143,135,215,112,238,141,177,229,39,252,19,63,254,9,221,96,113,242,240,60,235,41,127,26,252,234,117,42,123,108,203,79,230,255,0,211,212,206,184,165,203,13,47,123,126,71,224,191,252,19,123,246,116,135,198,191,2,63,97,105,154,193,88,234,223,179,7,193,173,76,200,209,72,91,19,124,5,211,175,11,183,151,203,22,150,121,177,181,190,234,141,203,201,175,215,175,248,98,216,63,231,202,63,251,241,125,199,83,253,255,0,127,214,188,7,254,9,7,227,143,9,105,191,179,55,252,19,232,94,218,219,92,221,89,126,200,223,2,173,165,243,75,28,52,127,179,182,139,12,138,202,89,142,67,239,60,96,115,157,163,154,253,217,63,21,188,21,255,0,64,173,59,160,199,203,39,255,0,27,165,143,196,85,142,38,170,73,219,154,95,152,169,198,60,171,174,223,146,63,41,63,225,139,96,255,0,159,24,255,0,239,197,247,255,0,23,71,252,49,108,31,243,227,31,253,248,190,255,0,226,235,245,111,254,22,183,130,127,232,21,167,127,223,50,127,241,186,63,225,107,120,39,254,129,90,119,253,243,39,255,0,27,174,37,137,175,213,59,187,126,134,156,177,236,126,82,127,195,22,193,255,0,62,49,255,0,223,139,239,254,46,143,248,98,216,63,231,198,63,251,241,125,255,0,197,215,234,223,252,45,111,4,255,0,208,43,78,255,0,190,100,255,0,227,116,127,194,214,240,79,253,2,180,239,251,230,79,254,55,66,196,98,58,167,119,111,208,57,99,216,252,164,255,0,134,45,131,254,124,99,255,0,191,23,223,252,93,31,240,197,176,127,207,140,127,247,226,251,255,0,139,175,213,191,248,90,222,9,255,0,160,86,157,255,0,124,201,255,0,198,232,255,0,133,173,224,159,250,5,105,223,247,204,159,252,110,133,136,196,117,78,238,223,160,114,199,177,249,73,255,0,12,91,7,252,248,199,255,0,126,47,191,248,186,63,225,139,96,255,0,159,24,255,0,239,197,247,255,0,23,95,171,127,240,181,188,19,255,0,64,173,59,254,249,147,255,0,141,209,255,0,11,91,193,63,244,10,211,191,239,153,63,248,221,11,17,136,234,157,221,191,64,229,143,99,242,147,254,24,182,15,249,241,143,254,252,95,127,241,116,127,195,22,193,255,0,62,49,255,0,223,139,239,254,46,191,86,255,0,225,107,120,39,254,129,90,119,253,243,39,255,0,27,163,254,22,183,130,127,232,21,167,127,223,50,127,241,186,22,35,17,213,59,187,126,129,203,30,199,229,39,252,49,108,31,243,227,31,253,248,190,255,0,226,235,227,223,218,107,246,75,183,209,62,49,127,193,63,45,141,146,149,241,7,237,117,227,141,33,209,97,186,6,69,181,253,129,127,110,15,18,20,30,100,128,19,159,15,130,57,28,174,73,11,154,254,135,127,225,107,120,39,254,129,90,119,253,243,39,255,0,27,175,130,127,108,159,137,30,18,188,248,223,255,0,4,199,146,222,202,202,213,116,255,0,219,139,226,37,245,204,145,135,4,219,143,248,38,111,252,20,74,211,99,103,104,219,231,221,192,121,59,114,163,112,43,145,93,56,76,69,103,82,106,73,235,78,167,225,77,145,40,198,203,78,171,243,71,227,23,237,79,251,36,104,214,90,70,167,20,250,69,176,146,56,76,32,203,20,106,170,138,130,54,140,174,248,195,194,100,146,92,58,0,235,246,175,49,70,236,61,125,23,255,0,6,249,126,213,223,18,244,63,137,126,61,255,0,130,108,120,143,78,240,238,183,240,179,225,215,195,63,136,63,31,254,13,248,166,194,56,244,141,127,192,182,114,124,87,240,109,175,142,62,28,234,118,182,186,90,199,226,221,15,83,241,79,198,89,53,235,43,235,153,83,82,210,175,87,87,179,150,227,84,210,239,244,123,79,13,253,3,251,91,248,227,195,109,99,172,249,70,218,32,203,36,121,181,123,124,74,205,29,196,96,19,39,149,230,99,206,192,198,236,164,108,195,3,0,255,0,50,119,63,176,231,197,143,248,42,31,237,53,172,126,206,223,0,188,67,240,239,194,30,54,240,167,195,255,0,17,124,103,212,117,79,140,26,191,137,124,63,225,121,252,47,225,175,20,120,71,193,247,246,22,87,190,11,240,143,136,110,228,215,223,83,248,165,225,247,138,39,177,138,221,160,179,188,119,186,142,72,225,138,227,232,50,90,245,157,68,172,218,107,110,251,25,84,73,41,35,245,195,246,12,253,165,108,52,31,216,111,246,50,209,30,238,84,125,23,246,81,253,157,180,151,85,104,176,173,166,252,32,240,125,161,81,153,193,24,104,143,108,251,118,164,253,157,191,105,109,63,78,248,189,251,121,94,53,220,170,53,223,218,187,194,26,170,16,209,124,194,31,216,111,246,50,208,247,54,110,63,191,163,56,227,251,128,103,57,199,243,135,251,36,254,211,191,18,188,85,251,55,105,242,124,44,248,99,241,35,226,111,132,191,102,63,129,255,0,15,159,227,119,138,190,30,120,79,197,62,50,240,215,193,223,14,232,94,3,186,103,241,15,197,45,111,195,122,29,213,167,195,205,8,105,190,8,241,101,201,187,213,166,180,183,22,254,24,212,38,243,60,187,43,150,139,200,60,1,255,0,5,34,240,103,135,252,85,241,187,87,151,196,122,60,105,227,159,138,26,79,138,109,93,245,251,88,214,120,44,254,11,252,33,240,83,73,19,21,253,242,11,175,7,220,33,97,192,104,153,58,161,171,121,108,221,76,91,246,109,251,91,219,255,0,3,131,253,1,78,202,62,246,223,240,199,236,239,236,95,251,79,218,124,61,253,159,127,99,180,159,83,251,58,104,159,179,151,193,221,40,254,249,222,80,34,248,59,163,89,34,132,137,193,85,73,227,184,94,10,48,5,122,142,107,238,227,255,0,5,0,209,251,107,178,254,34,239,39,60,228,226,239,147,146,127,42,249,251,254,8,41,255,0,4,104,241,87,237,101,240,103,225,15,237,49,251,113,248,87,196,30,24,253,151,224,248,79,225,109,51,224,79,193,11,141,83,197,30,11,241,191,199,180,127,6,105,154,77,167,198,31,22,234,122,6,167,97,171,120,31,224,165,186,27,187,159,10,91,218,220,219,106,158,51,186,158,223,196,41,46,159,224,139,93,44,252,66,250,7,194,31,179,79,252,19,251,198,63,240,83,111,31,126,196,58,119,236,47,224,249,62,16,248,95,246,186,248,71,240,127,194,127,28,108,255,0,104,127,219,11,82,255,0,133,147,240,191,199,159,240,79,175,219,163,227,39,196,43,63,14,75,7,199,197,211,38,248,129,224,159,219,99,246,11,248,173,224,93,111,87,182,184,190,211,163,254,202,215,252,21,125,225,253,43,197,190,19,212,181,107,142,170,153,55,181,156,170,74,74,60,207,109,221,156,149,182,95,240,196,41,217,91,95,234,194,127,195,127,233,31,244,30,147,255,0,39,63,249,46,143,248,111,253,35,254,131,210,127,228,231,255,0,37,215,238,31,252,56,119,254,9,81,255,0,70,193,127,255,0,137,5,251,79,127,243,232,163,254,28,59,255,0,4,168,255,0,163,96,191,255,0,196,130,253,167,191,249,244,84,127,96,197,90,211,95,213,188,188,135,237,61,79,195,207,248,111,253,35,254,131,210,127,228,231,255,0,37,209,255,0,13,255,0,164,127,208,122,79,252,156,255,0,228,186,253,195,255,0,135,14,255,0,193,42,63,232,216,47,255,0,241,32,191,105,239,254,125,20,127,195,135,127,224,149,31,244,108,23,255,0,248,144,95,180,247,255,0,62,138,127,216,49,255,0,159,139,250,183,151,144,123,79,83,240,243,254,27,255,0,72,255,0,160,244,159,249,57,255,0,201,116,127,195,127,233,31,244,30,147,255,0,39,63,249,46,191,112,255,0,225,195,191,240,74,143,250,54,11,255,0,252,72,47,218,123,255,0,159,69,31,240,225,223,248,37,71,253,27,5,255,0,254,36,23,237,61,255,0,207,162,143,236,24,255,0,207,197,253,91,203,200,61,167,169,248,121,255,0,13,255,0,164,127,208,122,79,252,156,255,0,228,186,63,225,191,244,143,250,15,73,255,0,147,159,252,151,95,184,127,240,225,223,248,37,71,253,27,5,255,0,254,36,23,237,61,255,0,207,162,143,248,112,239,252,18,163,254,141,130,255,0,255,0,18,11,246,158,255,0,231,209,71,246,12,127,231,226,254,173,229,228,30,211,212,252,60,255,0,134,255,0,210,63,232,61,39,254,78,127,242,93,31,240,223,250,71,253,7,164,255,0,201,207,254,75,175,220,63,248,112,239,252,18,163,254,141,130,255,0,255,0,18,11,246,158,255,0,231,209,71,252,56,123,254,9,83,255,0,70,193,127,255,0,137,5,251,79,127,243,232,161,100,49,255,0,159,139,238,244,255,0,32,246,158,167,225,231,252,55,254,145,255,0,65,233,63,242,115,255,0,146,235,230,159,142,223,183,6,149,172,252,82,253,139,181,21,214,164,117,240,159,237,47,226,175,16,72,199,237,127,186,75,159,216,235,246,177,240,168,147,155,158,166,79,19,70,156,16,127,121,215,25,7,235,127,218,115,246,94,255,0,130,107,124,23,253,184,117,79,217,15,193,191,177,134,153,226,123,57,52,31,248,39,182,163,164,120,246,95,140,95,181,126,179,225,125,11,196,255,0,27,127,224,163,127,3,63,101,63,218,255,0,225,55,142,252,81,166,126,213,54,139,167,124,95,209,62,0,126,216,159,177,247,141,60,39,160,67,166,27,157,54,219,226,181,183,137,60,80,110,244,77,115,195,26,126,167,250,101,241,99,254,13,239,255,0,130,126,248,147,199,191,179,30,179,240,243,224,38,153,161,248,71,225,255,0,199,13,127,197,223,29,116,205,111,227,231,237,57,46,165,227,31,133,87,191,179,111,237,9,224,45,31,195,158,16,118,248,141,124,109,252,69,23,198,255,0,27,124,27,213,229,117,185,209,201,210,124,43,170,71,253,163,42,185,210,117,74,165,146,194,50,147,114,73,164,227,182,252,208,74,233,181,182,191,122,183,112,115,186,211,250,219,252,143,231,95,197,191,180,247,143,63,105,47,27,248,63,224,151,192,191,15,106,255,0,19,126,41,124,83,212,211,195,222,10,240,95,135,110,109,36,213,53,141,77,108,238,110,238,195,221,53,228,54,186,78,131,103,166,105,215,55,218,165,245,212,182,214,90,110,153,109,123,169,106,55,54,122,125,165,221,244,31,212,15,252,17,251,254,9,173,167,254,197,63,8,35,248,165,241,135,193,186,68,63,182,175,198,109,42,238,127,140,126,33,254,223,176,241,164,191,15,188,45,121,175,75,172,120,115,224,167,131,53,219,29,58,43,109,35,73,182,176,182,240,245,223,138,87,78,155,81,135,86,241,93,172,236,53,237,115,65,209,124,36,116,223,210,239,132,223,179,191,236,255,0,240,19,251,127,254,20,95,192,207,131,191,5,255,0,225,43,254,202,255,0,132,167,254,21,55,195,47,5,124,57,255,0,132,151,251,11,251,75,251,19,251,127,254,16,253,18,207,251,99,236,127,219,58,191,217,126,209,230,125,159,251,86,231,201,217,231,203,187,216,171,210,194,96,40,225,21,225,239,78,214,191,249,16,228,223,145,249,123,255,0,5,14,253,137,190,41,254,214,203,227,213,248,117,226,15,135,250,39,252,37,63,240,76,95,248,41,151,236,93,167,159,26,234,158,34,211,252,159,138,127,182,92,223,178,92,191,11,181,219,209,161,120,87,81,242,254,31,216,255,0,194,133,241,112,215,238,211,204,212,109,63,180,180,211,167,105,58,175,155,117,246,63,212,42,40,174,219,43,223,171,36,43,224,15,135,63,242,148,223,219,35,254,204,3,254,9,167,255,0,173,21,255,0,5,98,175,191,235,248,131,255,0,130,254,127,193,106,255,0,106,127,248,35,231,252,21,53,63,225,154,60,3,240,3,199,31,240,209,31,176,7,236,183,255,0,9,175,252,47,79,11,124,69,241,55,246,95,252,42,79,218,43,246,239,255,0,132,115,254,17,111,248,64,62,42,248,99,236,94,127,252,44,221,123,237,223,107,251,119,153,246,75,63,35,236,222,92,223,104,96,127,111,148,87,249,130,127,196,106,223,240,84,223,250,32,127,176,7,254,26,207,218,43,255,0,162,170,143,248,141,91,254,10,155,255,0,68,15,246,0,255,0,195,89,251,69,127,244,85,80,7,250,125,209,95,230,9,255,0,17,171,127,193,83,127,232,129,254,192,31,248,107,63,104,175,254,138,170,63,226,53,111,248,42,111,253,16,63,216,3,255,0,13,103,237,21,255,0,209,85,64,31,233,247,69,127,152,39,252,70,173,255,0,5,77,255,0,162,7,251,0,127,225,172,253,162,191,250,42,168,255,0,136,213,191,224,169,191,244,64,255,0,96,15,252,53,159,180,87,255,0,69,85,0,127,167,221,21,254,96,159,241,26,183,252,21,55,254,136,31,236,1,255,0,134,179,246,138,255,0,232,170,163,254,35,86,255,0,130,166,255,0,209,3,253,128,63,240,214,126,209,95,253,21,84,1,254,159,116,87,249,130,127,196,106,223,240,84,223,250,32,127,176,7,254,26,207,218,43,255,0,162,170,143,248,141,91,254,10,155,255,0,68,15,246,0,255,0,195,89,251,69,127,244,85,80,7,247,249,241,27,254,82,155,251,27,255,0,217,128,127,193,75,63,245,162,191,224,147,181,247,253,127,16,127,240,64,79,248,45,95,237,79,255,0,5,131,255,0,130,166,191,252,52,191,128,126,0,120,31,254,25,223,246,0,253,169,63,225,10,255,0,133,23,225,111,136,190,25,254,212,255,0,133,183,251,69,126,194,31,240,145,255,0,194,83,255,0,9,255,0,197,95,19,253,183,200,255,0,133,101,160,253,135,236,159,97,242,254,215,121,231,253,167,204,135,236,255,0,219,229,0,20,81,69,0,127,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5295; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

