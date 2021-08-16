#!/usr/bin/env node

const fs = require("fs"), crypto = require("crypto");

const aes = "aes-192-cbc";
const scrypt_salt = "oiDBss7HQI";

// get the encryption key

let key;
{
  const pw = process.env["_9f8_pw"] ?? "";
  if (pw.length < 10) {
    console.error("please put a password in environment variable _9f8_pw");
    process.exit(1);
  }
  key = crypto.scryptSync(pw, scrypt_salt, 24);
}

// encrypt the compiler source code

const init_vec = crypto.randomBytes(16);
let encrypted;
{
  const compiler_src = fs.readFileSync("compiler.js");
  const cipher = crypto.createCipheriv(aes, key, init_vec);
  encrypted = Buffer.concat([cipher.update(compiler_src), cipher.final()]);
}

// create a new script which prints the compiler source code

process.stdout.write(`#!/usr/bin/env node
// note: this script was generated automatically and should not be modified
const fs = require("fs"), crypto = require("crypto");
const key = crypto.scryptSync(process.env["_9f8_pw"] ?? "", "${scrypt_salt}", 24);
const init_vec = Buffer.from(${JSON.stringify(init_vec.toString("base64"))}, "base64");
const encrypted = Buffer.from(${JSON.stringify(encrypted.toString("base64"))}, "base64");
const decipher = crypto.createDecipheriv("${aes}", key, init_vec);
const compiler_src = Buffer.concat([decipher.update(encrypted), decipher.final()]);
process.stdout.write(compiler_src);
process.stderr.write('successfully unpacked compiler source code\\n');
`);
process.stderr.write("successfully generated unpacking file\n");
