import java.io.*;
import java.lang.String;
import java.util.ArrayList;

public class id_mapper
{
   ArrayList item_list = new ArrayList();
   private int myOffset;

   public id_mapper(String directory, int offset) throws IOException
   {
      myOffset = offset;
      String item_name = new String();

      FileInputStream configFile = new FileInputStream(directory);
      BufferedReader r = new BufferedReader(new InputStreamReader(configFile));

      while ((item_name = r.readLine()) != null)
      {
         //System.out.println(item_name);
         item_list.add(item_name);
      }

      r.close();
   }

   public int get_id(String in)
   {
      return item_list.indexOf((Object)in) + myOffset;
   }

   public String get_string(int id)
   {
      return (String)item_list.get(id);
   }
}

