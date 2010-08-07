package com.contentwatch.nameserver;

import java.io.IOException;

import com.ericsson.otp.erlang.*;

public class NameServerClient {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			// first, open a shell and start an erlang node with the shortname of "server"
			// 	erl -sname server
			//
			OtpSelf client = new OtpSelf("client");
			OtpPeer server = new OtpPeer("server");
			OtpConnection conn = client.connect(server);
			conn.sendRPC("name_server", "start", new OtpErlangList());
			System.out.println("Starting name_server, response = " + conn.receiveMsg().getMsg().toString());
			OtpErlangAtom weather 	= new OtpErlangAtom("weather");
			OtpErlangAtom fine		= new OtpErlangAtom("fine");
			OtpErlangAtom parmlist[] = {weather, fine};
			conn.sendRPC("name_server", "store", new OtpErlangList(parmlist));
			System.out.println("name_server:store, response = " + conn.receiveMsg().getMsg().toString());
			conn.sendRPC("name_server", "lookup", new OtpErlangList(new OtpErlangAtom("weather")));
			System.out.println("name_server:lookup, response = " + conn.receiveMsg().getMsg().toString());
			conn.send("name_server", new OtpErlangAtom("stop"));
			System.out.println("Stopped name_server");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpAuthException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangExit e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}

}
