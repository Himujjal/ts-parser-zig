const fs = require("fs");
const path = require("path");

const files = [];

function populate(root) {
  const dirs = fs.readdirSync(root);
  for (const dir of dirs) {
    const file_folder = path.resolve(root, dir);
    if (fs.lstatSync(file_folder).isDirectory()) {
      populate(file_folder);
    } else {
      if (path.extname(file_folder) == ".json") {
        files.push(file_folder);
      }
    }
  }
}

populate("fixtures");

// function createTSFile() {
//   files.forEach(file => {
//   	const content = fs.readFileSync(file);
//   	const ext = path.extname(file)
//   	const newFile = file.slice(0, file.length - ext.length) + '.ts';
//   	fs.writeFileSync(newFile, content);
//   })
// }

function renameJSON5File() {
	files.forEach(file => {
		const content = fs.readFileSync(file);
		const ext = path.extname(file);
		const newFile = file.slice(0, file.length - ext.length) + ".json5";
		fs.writeFileSync(newFile, content);

		fs.rmSync(file);
	})
}

renameJSON5File();
