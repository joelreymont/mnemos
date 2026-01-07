export class MnemosClient {
  // Placeholder: connect to Lisp backend via WebSocket or HTTP.

  async explainRegion(params: {
    file: string;
    start: { line: number; column: number };
    end: { line: number; column: number };
  }): Promise<any> {
    // TODO: JSON-RPC call to mnemos.explain-region
    console.log("explainRegion called with", params);
    return { explanation: "Stub — connect to backend." };
  }
}
