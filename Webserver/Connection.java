/**
 * This is the separate thread that services each
 * incoming echo client request.
 *
 * @author Greg Gagne
 *
 * Used for HW3 Networks
 */

import java.net.*;
import java.io.*;
import java.net.URL;
import java.util.Calendar;


public class Connection implements Runnable
{
	public static final int BUFFER_SIZE = 2048;
	public static final int PORT = 8080;


	private Socket client;
	private Configuration config;
	//private static Handler handler = new Handler();

	public Connection(Socket client, Configuration config) {
		this.client = client;
		this.config = config;
	}

    /**
     * This method runs in a separate thread.
     */
	public void run() {
		try{
			handler();
		}//end try
		catch (IOException ioe) {
			System.err.println(ioe);
		}
	}

	public int findChar(char c, int pos, String s) {
			int index = pos;

			while (s.charAt(pos) != c && index < s.length())
					pos++;

			System.out.println("request = " + s + " pos = " + pos);
			return pos;
	}

	public void handler() throws IOException {
		try {

			String[] contentType = {"text/html", "image/gif", "image/jpeg", "image/png", "text/plain"};

			try{

				BufferedReader is = new BufferedReader(new InputStreamReader(client.getInputStream()));
				OutputStream os = new BufferedOutputStream(client.getOutputStream());

				//Set up the request, parsing:
				String innLine = is.readLine();
				String[] part = innLine.split(" ");

/*
				System.out.println(part.length);
				System.out.println("This is what's beeing read in from InputStream: " + innLine);
*/

//No get, close and say goodby
				if (innLine == null || !innLine.substring(0,3).equals("GET") ){
					client.close();
					return;
				}

				String originPost = " ";
				String fileType;
				String req;
				String date;
				String server;
				String content = " ";
				String contentSize;
				String kobling;
				String beOm;

				File requestedFile = null;
				long contentSizeLong = 0;

				if(part.length == 3){
					originPost = part[1];
				}

				if(originPost.length() > 1){
					// get fileType
					fileType = originPost.substring(originPost.lastIndexOf("."));
					requestedFile = new File(config.getDocumentRoot() + "/" + originPost);

					//System.out.println("What is fileType at this point: " + fileType);
					//make content the fileType
					if(fileType.equals(".html")){
						content = contentType[0];
					}
					else if(fileType.equals(".gif")){
						content = contentType[1];
					}
					else if(fileType.equals(".jpeg")){
						content = contentType[2];
					}
					else if(fileType.equals(".png")){
						content = contentType[3];
					}
					else if(fileType.equals(".txt")){
						content = contentType[4];
					}
				}
				else if(originPost.length() == 1 && originPost.equals("/")){
					content =  contentType[0];
					requestedFile = new File(config.getDefaultDocument());

				}


				if(requestedFile.isFile()){
					req = "HTTP/1.1 200 OK \r\n";
					contentSizeLong = requestedFile.length();
				}
				else{
					req = "HTTP/1.1 404 NOT FOUND \r\n";
					requestedFile = new File(config.getDefaultDocument());
					contentSizeLong = requestedFile.length();
				}


				/*write the HEADERS
				* HTTP/1.1 <200 OK/ 404 NOT fund>
				* Date: <todays date>
				* Server Name: <Server Name (from config)>
				* Content-Type: <contentType[x]>
				* Content-Lenght: contentType[x].length
				* Connection: closed
				*/
				//req
				date = "Date: "+ Calendar.getInstance().getTime() + "\r\n";
				server = "Server: " + config.getServerName() + "\r\n";
				content = "Content-Type: " + content + "\r\n";
				contentSize = "Content-Size: " + String.valueOf(contentSizeLong) + "\r\n";
				kobling = "Connection: closed\r\n\r\n";

				beOm = req + date + server + content + contentSize + kobling;
				System.out.println("at this point the request is: \n" + beOm);

				os.write(beOm.getBytes());
				os.flush();

				System.out.println("Yoyoy we are past the flush");
				

				InputStream fileIn = new BufferedInputStream(new FileInputStream(requestedFile));
				while(fileIn.available() > 0){
					//System.out.println(fileIn.read());
					os.write(fileIn.read());
					os.flush();
				}

//close
				// PrintWritter writer = new PrintWritter(config.getLogFile());
				// writer.write("" + client.getInetAddress() + " [" + date + "]" + " ''" + req + "''" + " " + part[0] + " " + originPost);
				os.close();
				is.close();
				client.close();

			}//end try
			catch (Exception e){
				System.err.println(e);
			}
			finally{
				client.close();
			}
		}//end try
		catch (java.io.IOException ioe) {
			System.err.println(ioe);
		}
	}
}//end of connection
