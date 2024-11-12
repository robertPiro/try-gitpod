# try-gitpod
Trying out gitpod in late 2024

## VScala JS
Create Vite project in current directory
```npm create vite@latest ./ --template vanilla --variant javascript```

Install NPM infrastructure
```npm install```

Start Vite's continuous compiling
```npm run dev```

Start SBT
```sbt
~fastLinkJS```

### Import ScalaJS output in application

Replace everything in `main.js` (except the `style.css` import) with the following line
```import './target/scala-3.5.2/livechart-fastopt/main.js'```

## Run after startup
- Open one bash and run `$npm run dev` to start Vite
- Open another bash and run `sbt ~fastLinkJS`
- Develop!

## Turn down AWS Elastic Container Usage
1. Elastic Container Service -> Clusters -> GitPod... 
2. At bottom go to Services, follow link GitPod ...
3. Got to Update in service and set Desired Tasks to 1
4. Confirm with update at the bottom
