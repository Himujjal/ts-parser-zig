const fs = require("fs");
const path = require("path");

const files = [];
function rename(root) {
  const dirs = fs.readdirSync(root);
  for (const dir of dirs) {
    const file_folder = path.resolve(root, dir);
    if (fs.lstatSync(file_folder).isDirectory()) {
      rename(file_folder);
    } else {
      if (path.extname(file_folder) == ".js") {
        files.push(file_folder);
      }
    }
  }
}

rename("tests/fixtures");

files.forEach(file => {
	const content = fs.readFileSync(file);
	const ext = path.extname(file)
	const newFile = file.slice(0, file.length - ext.length) + '.ts';
	fs.writeFileSync(newFile, content);
})

// console.log(files.slice(0, 10));
