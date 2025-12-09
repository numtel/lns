// tools/build.js
import { promises as fs } from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const ROOT = path.resolve(__dirname, "..");
const SRC = path.join(ROOT, "src");
const DIST = path.join(ROOT, "dist");

// Read first-line meta: <!-- key: value -->
function parseMetaAndContent(raw) {
  const meta = {};
  const commentMatch = raw.match(/^<!--([\s\S]*?)-->\s*/);

  if (commentMatch) {
    const block = commentMatch[1].trim();
    const lines = block.split(/\r?\n/);

    let currentKey = null;
    for (let line of lines) {
      const keyMatch = line.match(/^\s*([a-zA-Z0-9_-]+)\s*:\s*(.*)$/);
      if (keyMatch) {
        currentKey = keyMatch[1].trim();
        meta[currentKey] = keyMatch[2];
      } else if (currentKey) {
        // continuation line — append preserving line breaks
        meta[currentKey] += "\n" + line;
      }
    }

    raw = raw.slice(commentMatch[0].length); // strip meta block from content
  }

  return { meta, content: raw };
}

async function copyDir(from, to) {
  try {
    await fs.mkdir(to, { recursive: true });
    const entries = await fs.readdir(from, { withFileTypes: true });
    for (const e of entries) {
      const src = path.join(from, e.name);
      const dst = path.join(to, e.name);
      if (e.isDirectory()) await copyDir(src, dst);
      else if (e.isFile()) await fs.copyFile(src, dst);
    }
  } catch (err) {
    if (err.code === "ENOENT") return; // optional folders
    throw err;
  }
}

async function getHtmlFilesRecursive(dir) {
  const entries = await fs.readdir(dir, { withFileTypes: true });
  const files = [];

  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      const subFiles = await getHtmlFilesRecursive(fullPath);
      files.push(...subFiles);
    } else if (entry.isFile() && entry.name.endsWith(".html")) {
      files.push(fullPath);
    }
  }
  return files;
}

async function build() {
  await fs.rm(DIST, { recursive: true, force: true });
  await fs.mkdir(DIST, { recursive: true });

  const template = await fs.readFile(path.join(SRC, "template.html"), "utf8");
  const pagesDir = path.join(SRC, "pages");
  const htmlFiles = await getHtmlFilesRecursive(pagesDir);

  for (const filePath of htmlFiles) {
    const relPath = path.relative(pagesDir, filePath);
    const raw = await fs.readFile(filePath, "utf8");
    const { meta, content } = parseMetaAndContent(raw);

    const title =
      meta.title ||
      path
        .basename(relPath, ".html")
        .replace(/[-_]/g, " ")
        .replace(/\b\w/g, (s) => s.toUpperCase());

    const html = template
      .replaceAll("{{title}}", meta.title || title)
      .replace("{{content}}", content);

    const outPath = path.join(DIST, relPath);
    await fs.mkdir(path.dirname(outPath), { recursive: true });
    await fs.writeFile(outPath, html, "utf8");
  }

  await copyDir(path.join(SRC, "assets"), path.join(DIST, "assets"));
  await copyDir(path.join(ROOT, "public"), DIST);

  console.log("Built site to", DIST);
}

async function exists(p) {
  try {
    await fs.access(p);
    return true;
  } catch {
    return false;
  }
}

build().catch((e) => {
  console.error(e);
  process.exit(1);
});

