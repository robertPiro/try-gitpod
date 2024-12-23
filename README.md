# try-gitpod
Trying out gitpod in late 2024

## Video Resources
- https://www.youtube.com/watch?v=hWUAVrNj65c

## Scala JS
Create sbt Project

Create Vite project in current directory (Ignore files and continue.)
```npm create vite@latest ./ --template vanilla --variant javascript```

Install NPM infrastructure
```npm install```

Start Vite's continuous compiling
```npm run dev```

Start SBT
```sbt ~fastLinkJS```

### Import ScalaJS output in application

Replace everything in `main.js` (except the `style.css` import) with the following line
```import './target/scala-3.5.2/livechart-fastopt/main.js'```

## Run after startup
- Open one bash and run `$npm run dev` to start Vite
- Open another bash and run `sbt ~fastLinkJS`
- Develop!

